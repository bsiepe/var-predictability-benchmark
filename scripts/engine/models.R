#----------------- Models ------------------------------
# This script contains the raw model definitions and the model registry.
# For each model, we define a fit() and predict() function. The model registry is a list of
# model definitions

# function to subset modeldata to a given set of indices (e.g. for cross-validation)
subset_modeldata <- function(md, idx) {
  list(
    id = md$id,
    Y = md$Y[idx, , drop = FALSE],
    Ylag = md$Ylag[idx, , drop = FALSE],
    time = md$time[idx],
    valid = md$valid[idx]
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

# Use a linear trend model
trend_fit <- function(train, spec) {
# TODO
}

trend_predict <- function(fitted, test) {
# TODO
}

# Use a random-intercept model
ri_fit <- function(train, spec) {
# TODO
}

ri_predict <- function(fitted, test) {
# TODO
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



# Multilevel autoregressive model
ml_ar_fit <- function(train, spec) {
  # TODO
}

ml_ar_predict <- function(fitted, test) {
  # TODO
}


#--------------- VAR models
var_fit <- function(train, spec) {
  # TODO
}

var_predict <- function(fitted, test) {
  # TODO
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
  ar = list(label = "AR(1)", level = "person", fit = ar_fit, predict = ar_predict)
)