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

# Last observation carried forward: Yhat[t] = Y[t-1]
locf_fit <- function(train, spec) list()

locf_predict <- function(fitted, test) {
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



# ml AR(1): common AR slope across persons, person-specific intercepts and slopes
# fitted by lmer with random intercept and random slope on the lagged variable
ml_ar_fit <- function(train_persons, spec) {
  # construct a data frame with all valid observations from all persons, including lagged variables
  rows <- lapply(train_persons, function(p) {
    valid <- p$valid
    lag_df <- as.data.frame(p$Ylag[valid, , drop = FALSE])
    colnames(lag_df) <- paste0(colnames(lag_df), "_lag")
    cbind(data.frame(id = p$id, p$Y[valid, , drop = FALSE], check.names = FALSE), lag_df)
  })
  df <- dplyr::bind_rows(rows)
  vars <- colnames(train_persons[[1]]$Y)
  fits <- list()
  # fit for each variable separately. lme4 warnings propagate to run_dataset(), which logs and counts them
  for (v in vars) {
    lag_v <- paste0(v, "_lag")
    fits[[v]] <- lme4::lmer(
      stats::as.formula(paste0("`", v, "` ~ 1 + `", lag_v, "` + (1 + `", lag_v, "` | id)")),
      data = df
    )
  }
  list(models = fits)
}

ml_ar_predict <- function(fitted, test_person) {
  vars <- names(fitted$models)
  lag_df <- as.data.frame(test_person$Ylag)
  colnames(lag_df) <- paste0(vars, "_lag")
  newdf <- data.frame(id = test_person$id, lag_df, check.names = FALSE)
  Yhat <- matrix(NA_real_, nrow = nrow(test_person$Y), ncol = length(vars),
                 dimnames = dimnames(test_person$Y))
  for (v in vars) {
    Yhat[, v] <- predict(fitted$models[[v]], newdata = newdf, allow.new.levels = TRUE)
  }
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

# builds the lmer formula string for one outcome variable in ml_var
.ml_var_formula <- function(v, lag_vars, re_corr) {
  sep   <- if (re_corr) " | " else " || "
  fixed <- paste0("`", lag_vars, "`", collapse = " + ")
  rand  <- paste0("(1 + ", paste0("`", lag_vars, "`", collapse = " + "), sep, "id)")
  stats::as.formula(paste0("`", v, "` ~ 1 + ", fixed, " + ", rand))
}

ml_var_fit <- function(train_persons, spec) {
  re_corr <- if (!is.null(spec$re_corr)) spec$re_corr else TRUE
  rows <- lapply(train_persons, function(p) {
    valid <- p$valid
    lag_df <- as.data.frame(p$Ylag[valid, , drop = FALSE])
    colnames(lag_df) <- paste0(colnames(lag_df), "_lag")
    cbind(data.frame(id = p$id, p$Y[valid, , drop = FALSE], check.names = FALSE), lag_df)
  })
  df <- dplyr::bind_rows(rows)
  vars <- colnames(train_persons[[1]]$Y)
  lag_vars <- paste0(vars, "_lag")
  fits <- list()
  # lme4 warnings propagate to run_dataset(), which logs and counts them
  for (v in vars) {
    fits[[v]] <- lme4::lmer(.ml_var_formula(v, lag_vars, re_corr), data = df)
  }
  list(models = fits)
}

ml_var_predict <- function(fitted, test_person) {
  vars <- names(fitted$models)
  # select and order Ylag columns by fitted variable names to guard against permutations
  lag_df <- as.data.frame(test_person$Ylag[, vars, drop = FALSE])
  colnames(lag_df) <- paste0(vars, "_lag")
  newdf <- data.frame(id = test_person$id, lag_df, check.names = FALSE)
  Yhat <- matrix(NA_real_, nrow = nrow(test_person$Y), ncol = length(vars),
                 dimnames = dimnames(test_person$Y))
  for (v in vars) {
    Yhat[, v] <- predict(fitted$models[[v]], newdata = newdf, allow.new.levels = TRUE)
  }
  Yhat
}


#----------- Model registry
model_registry <- list(
  mean  = list(label = "Person mean", level = "person",  fit = mean_fit, predict = mean_predict),
  locf  = list(label = "LOCF", level = "person", fit = locf_fit, predict = locf_predict),
  trend = list(label = "Deterministic trend", level = "person",  fit = trend_fit, predict = trend_predict),
  ri    = list(label = "Random intercept", level = "dataset", fit = ri_fit,  predict = ri_predict),
  ar    = list(label = "AR(1)",  level = "person",  fit = ar_fit, predict = ar_predict),
  var   = list(label = "VAR(1)",  level = "person",  fit = var_fit,   predict = var_predict),
  ml_ar = list(label = "Multilevel AR(1)",  level = "dataset", fit = ml_ar_fit,   predict = ml_ar_predict),
  ml_var = list(label = "Multilevel VAR(1)",   level = "dataset", fit = ml_var_fit,  predict = ml_var_predict)
)