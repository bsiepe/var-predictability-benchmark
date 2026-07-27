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

#------------ Autoregressive models
# Person-specific autoregressive model fit by OLS
ar_fit <- function(train, spec) {
  rows <- train$valid
  Y <- train$Y[rows, , drop = FALSE]
  Yl <- train$Ylag[rows, , drop = FALSE]
  coefs <- vapply(seq_len(ncol(Y)), function(v) {
    stats::lm.fit(cbind(1, Yl[, v]), Y[, v])$coefficients
  }, numeric(2))
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
  # fit for each variable separately
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
  list(coefs = stats::lm.fit(cbind(1, Yl), Y)$coefficients)
}

var_predict <- function(fitted, test) {
  Yhat <- cbind(1, test$Ylag) %*% fitted$coefs
  dimnames(Yhat) <- dimnames(test$Y)
  Yhat
}

ml_var_fit <- function(train, spec) {
  # TODO
}

ml_var_predict <- function(fitted, test) {
  # TODO
}


#----------- Model registry
model_registry <- list(
  mean = list(label = "Person mean", level = "person", fit = mean_fit, predict = mean_predict),
  trend = list(label = "Deterministic trend", level = "person", fit = trend_fit, predict = trend_predict),
  ri = list(label = "Random intercept", level = "dataset", fit = ri_fit, predict = ri_predict),
  ar = list(label = "AR(1)", level = "person", fit = ar_fit, predict = ar_predict),
  var = list(label = "VAR(1)", level = "person", fit = var_fit, predict = var_predict),
  ml_ar = list(label = "Multilevel AR(1)", level = "dataset", fit = ml_ar_fit, predict = ml_ar_predict)
)