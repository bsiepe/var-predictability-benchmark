#----------------- Models ------------------------------
# This script contains the raw model definitions and the model registry.
# For each model, we define a fit() and predict() function. The model registry is a list of
# model definitions

# function to subset modeldata to a given set of indices (e.g. for cross-validation)
subset_modeldata <- function(person_data, tps) {
  list(
    id = person_data$id,
    Y = person_data$Y[tps, , drop = FALSE],
    Ylag = person_data$Ylag[tps, , drop = FALSE],
    time = person_data$time[tps],
    valid = person_data$valid[tps]
  )
}


#------------- Baseline models
# Use the simple mean of the training data as a baseline model
mean_fit <- function(train, spec) {
  list(mu = colMeans(train$Y[train$valid, , drop = FALSE], na.rm = TRUE))
}

mean_predict <- function(fitted, test) {
  matrix(fitted$mu, nrow = nrow(test$Y), ncol = ncol(test$Y), byrow = TRUE,
         dimnames = dimnames(test$Y))
}

# naive (random-walk) forecast: Yhat[t] = Y[t-1]
naive_fit <- function(train, spec) list()

naive_predict <- function(fitted, test) {
  Yhat <- test$Ylag
  dimnames(Yhat) <- dimnames(test$Y)
  Yhat
}

# OLS fit of y ~ intercept + slope * t for a single variable
.ols_trend <- function(y, t) stats::lm.fit(cbind(1, t), y)$coefficients

# Person-specific linear trend: Y ~ intercept + slope * time
trend_fit <- function(train, spec) {
  rows <- train$valid
  Y <- train$Y[rows, , drop = FALSE]
  t <- train$time[rows]
  coefs <- matrix(NA_real_, nrow = 2, ncol = ncol(Y), dimnames = list(NULL, colnames(Y)))
  for (v in colnames(Y)) coefs[, v] <- .ols_trend(Y[, v], t)
  list(coefs = coefs)
}

trend_predict <- function(fitted, test) {
  a <- fitted$coefs[1, ]
  b <- fitted$coefs[2, ]
  Yhat <- matrix(NA_real_, nrow = nrow(test$Y), ncol = ncol(test$Y),
                 dimnames = dimnames(test$Y))
  for (v in colnames(Yhat)) Yhat[, v] <- a[v] + b[v] * test$time
  Yhat
}

# random-intercept model
ri_fit <- function(train_persons, spec) {
  rows <- lapply(train_persons, function(p) {
    valid <- p$valid
    data.frame(id = p$id, p$Y[valid, , drop = FALSE], check.names = FALSE)
  })
  df <- dplyr::bind_rows(rows)
  vars <- colnames(train_persons[[1]]$Y)
  fits <- list()
  for (v in vars) {
    fits[[v]] <- lme4::lmer(stats::as.formula(paste0(v, " ~ 1 + (1|id)")), data = df)
  }
  list(models = fits)
}

ri_predict <- function(fitted, test_person) {
  vars <- names(fitted$models)
  newdf <- data.frame(id = test_person$id)
  Yhat <- matrix(NA_real_, nrow = nrow(test_person$Y), ncol = length(vars),
                 dimnames = dimnames(test_person$Y))
  for (v in vars) {
    pred <- predict(fitted$models[[v]], newdata = newdf, allow.new.levels = TRUE)
    Yhat[, v] <- pred
  }
  Yhat
}

#------------ OLS helper
# least-squares coefficients with aliased coefficients set to 0. a column is aliased when it is
# collinear with others in the training window (e.g. a constant lagged item, or p + 1 > n).
# setting it to 0 gives the least-squares solution on the reduced design, as predict.lm() does
.ols_coefs <- function(X, Y) {
  coefs <- stats::lm.fit(X, Y)$coefficients
  aliased <- is.na(coefs)
  coefs[aliased] <- 0
  list(coefs = coefs, aliased = any(aliased))
}

# raised at most once per fit call, so counts equal the number of affected fits
.warn_rank_deficient <- function() {
  warning("rank-deficient OLS fit: aliased coefficients set to 0", call. = FALSE)
}

#------------ Autoregressive models
# Person-specific autoregressive model fit by OLS
ar_fit <- function(train, spec) {
  rows <- train$valid
  Y <- train$Y[rows, , drop = FALSE]
  Yl <- train$Ylag[rows, , drop = FALSE]
  fits <- lapply(seq_len(ncol(Y)), function(v) .ols_coefs(cbind(1, Yl[, v]), Y[, v]))
  if (any(vapply(fits, `[[`, logical(1), "aliased"))) .warn_rank_deficient()
  coefs <- vapply(fits, `[[`, numeric(2), "coefs")
  colnames(coefs) <- colnames(Y)
  list(coefs = coefs)
}

ar_predict <- function(fitted, test) {
  a <- fitted$coefs[1, ]
  phi <- fitted$coefs[2, ]
  Yhat <- sweep(sweep(test$Ylag, 2, phi, `*`), 2, a, `+`)
  dimnames(Yhat) <- dimnames(test$Y)
  Yhat
}


#--------------- VAR models
# Person-specific VAR(1) fit by OLS; coefs is a (p+1) x p matrix
# (row 1: intercepts, rows 2:(p+1): lagged coefficient matrix Phi)
var_fit <- function(train, spec) {
  rows <- train$valid
  Y <- train$Y[rows, , drop = FALSE]
  Yl <- train$Ylag[rows, , drop = FALSE]
  fit <- .ols_coefs(cbind(1, Yl), Y)
  if (fit$aliased) .warn_rank_deficient()
  list(coefs = fit$coefs)
}

var_predict <- function(fitted, test) {
  Yhat <- cbind(1, test$Ylag) %*% fitted$coefs
  dimnames(Yhat) <- dimnames(test$Y)
  Yhat
}

#--------------- Regularised VAR
# by-equation LASSO with lambda chosen by EBIC on the training window, so the selection never sees
# the test point. returns the intercept followed by the lag coefficients for one equation, and
# whether the selection sits at the smallest admissible lambda (end of the path or the df cap),
# where a longer path might have chosen a denser model
.lasso_coefs <- function(X, y, own, spec) {
  # glmnet stops on a constant response or when no predictor varies; the empty model is the mean
  if (stats::var(y) == 0 || all(apply(X, 2, stats::var) == 0)) {
    return(list(coefs = c(mean(y), numeric(ncol(X))), at_boundary = FALSE))
  }
  penalized <- rep(TRUE, ncol(X))
  if (!spec$penalize_ar) penalized[own] <- FALSE
  n <- length(y)
  fit <- glmnet::glmnet(X, y, alpha = 1, penalty.factor = as.numeric(penalized),
                        lambda.min.ratio = spec$lambda_min_ratio)
  # residual sum of squares at each lambda for the gaussian family
  rss <- stats::deviance(fit)
  # EBIC as in graphicalVAR, restricted to beta (no kappa here); gamma = 0 is BIC:
  # https://github.com/SachaEpskamp/graphicalVAR/blob/11c503143e69b2c6a46cff527370c0ff4c66b921/R/Rothmana.R#L105-L115
  # n_selected is pdB there: penalized nonzero slopes only. an unpenalized own lag is in every
  # path point or in none, so leaving it out shifts all values equally.
  # n_vars is nY there; our X has no intercept column, so ncol(X) is the number of variables
  n_selected <- colSums(as.matrix(fit$beta[penalized, , drop = FALSE]) != 0)
  n_vars <- ncol(X)
  ebic <- n * log(rss / n) + n_selected * (log(n) + 4 * spec$ebic_gamma * log(2 * n_vars))
  # fit$df counts slopes only, the + 1 is the intercept
  ebic[n - (fit$df + 1) < spec$min_resid_df] <- Inf
  # the first lambda has no penalized slopes, so it always passes the cap
  stopifnot(!anyNA(ebic), ebic[1] < Inf)
  best <- which.min(ebic)
  list(coefs = c(fit$a0[best], fit$beta[, best]), at_boundary = best == max(which(ebic < Inf)))
}

# person-specific VAR(1) by LASSO; same coefs layout as var_fit, so var_predict applies
lasso_var_fit <- function(train, spec) {
  rows <- train$valid
  Y <- train$Y[rows, , drop = FALSE]
  X <- train$Ylag[rows, , drop = FALSE]
  coefs <- matrix(NA_real_, nrow = ncol(X) + 1, ncol = ncol(Y), dimnames = list(NULL, colnames(Y)))
  at_boundary <- FALSE
  for (v in seq_len(ncol(Y))) {
    equation <- .lasso_coefs(X, Y[, v], v, spec)
    coefs[, v] <- equation$coefs
    at_boundary <- at_boundary || equation$at_boundary
  }
  # raised at most once per fit call, so counts equal the number of affected fits
  if (at_boundary) warning("lasso selection at the smallest admissible lambda", call. = FALSE)
  list(coefs = coefs)
}

#--------------- Multilevel models
.ml_centring <- function(spec) {
  center_lag <- match.arg(spec$center_lag %||% "person", c("none", "person"))
  between <- isTRUE(spec$between)
  if (between && center_lag != "person")
    stop("between = TRUE requires center_lag = \"person\"", call. = FALSE)
  list(center_lag = center_lag, between = between)
}

# person x item matrix of the person means of the lags over the training rows. computed at every
# fit, so test rows never enter a centre
.lag_centres <- function(train_persons) {
  centres <- do.call(rbind, lapply(train_persons, function(p) {
    colMeans(p$Ylag[p$valid, , drop = FALSE])
  }))
  rownames(centres) <- vapply(train_persons, function(p) as.character(p$id), character(1))
  # an empty training window gives NaN, which would otherwise surface as NaN predictions
  if (!all(is.finite(centres))) stop("non-finite lag centre: empty training window", call. = FALSE)
  centres
}

# lag columns of person p for lmer, centred on the person's training mean if centres is given.
# between = TRUE adds that mean as a person-level predictor
.ml_predictors <- function(p, rows, vars, centres, between) {
  lags <- p$Ylag[rows, vars, drop = FALSE]
  centre <- NULL
  if (!is.null(centres)) {
    # use this person's training-window means, ordered to match the lag columns.
    centre <- centres[as.character(p$id), vars, drop = TRUE]
    # subtract each item's mean from its lag column
    lags <- sweep(lags, 2, centre)
  }

  predictors <- as.data.frame(lags)
  names(predictors) <- paste0(vars, "_lag")
  if (between) {
    means <- as.data.frame(lapply(centre, rep, times = nrow(lags)))
    names(means) <- paste0(vars, "_lagmean")
    predictors <- cbind(predictors, means)
  }
  predictors
}

# builds the lmer formula for one outcome variable
.ml_formula <- function(v, lag_vars, re_corr, between_vars = NULL) {
  sep <- if (re_corr) " | " else " || "
  fixed <- paste0("`", c(lag_vars, between_vars), "`", collapse = " + ")
  rand <- paste0("(1 + ", paste0("`", lag_vars, "`", collapse = " + "), sep, "id)")
  stats::as.formula(paste0("`", v, "` ~ 1 + ", fixed, " + ", rand))
}

# one lmer per item with a random intercept and random slopes on the lags: the own lag for ml_ar,
# all lags for ml_var
.ml_fit <- function(train_persons, spec, own_lag_only) {
  re_corr <- spec$re_corr %||% TRUE
  centring <- .ml_centring(spec)
  centres <- if (centring$center_lag == "person") .lag_centres(train_persons)
  vars <- colnames(train_persons[[1]]$Y)
  rows <- lapply(train_persons, function(p) {
    cbind(data.frame(id = p$id, p$Y[p$valid, , drop = FALSE], check.names = FALSE),
          .ml_predictors(p, p$valid, vars, centres, centring$between))
  })
  # an item named like another item's lag column would silently stand in for it in the formula
  stopifnot(!anyDuplicated(names(rows[[1]])))
  df <- dplyr::bind_rows(rows)
  fits <- list()
  # lme4 warnings propagate to run_dataset(), which logs and counts them
  for (v in vars) {
    predictors <- if (own_lag_only) v else vars
    between_vars <- if (centring$between) paste0(predictors, "_lagmean")
    fits[[v]] <- lme4::lmer(.ml_formula(v, paste0(predictors, "_lag"), re_corr, between_vars),
                            data = df)
  }
  list(models = fits, centres = centres, between = centring$between)
}

ml_ar_fit <- function(train_persons, spec) .ml_fit(train_persons, spec, own_lag_only = TRUE)
ml_var_fit <- function(train_persons, spec) .ml_fit(train_persons, spec, own_lag_only = FALSE)

# Ylag columns are selected by the fitted variable names, which guards against permutations
.ml_predict <- function(fitted, test_person) {
  vars <- names(fitted$models)
  predictors <- .ml_predictors(test_person, seq_len(nrow(test_person$Ylag)), vars,
                               fitted$centres, fitted$between)
  newdf <- data.frame(id = test_person$id, predictors, check.names = FALSE)
  Yhat <- matrix(NA_real_, nrow = nrow(test_person$Y), ncol = length(vars),
                 dimnames = dimnames(test_person$Y))
  for (v in vars) {
    Yhat[, v] <- stats::predict(fitted$models[[v]], newdata = newdf, allow.new.levels = TRUE)
  }
  Yhat
}

ml_ar_predict <- .ml_predict
ml_var_predict <- .ml_predict


#----------- Model registry
model_registry <- list(
  mean = list(label = "Person mean", level = "person",  fit = mean_fit, predict = mean_predict),
  naive = list(label = "Naive", level = "person", fit = naive_fit, predict = naive_predict),
  trend = list(label = "Deterministic trend", level = "person",  fit = trend_fit, predict = trend_predict),
  ri = list(label = "Random intercept", level = "dataset", fit = ri_fit,  predict = ri_predict),
  ar = list(label = "AR(1)",  level = "person",  fit = ar_fit, predict = ar_predict),
  var = list(label = "VAR(1)",  level = "person",  fit = var_fit,   predict = var_predict),
  lasso_var = list(label = "LASSO VAR(1)", level = "person", fit = lasso_var_fit, predict = var_predict),
  ml_ar = list(label = "Multilevel AR(1)",  level = "dataset", fit = ml_ar_fit,   predict = ml_ar_predict),
  ml_var = list(label = "Multilevel VAR(1)",   level = "dataset", fit = ml_var_fit,  predict = ml_var_predict)
)