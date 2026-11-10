# Tests for person-mean centring of the lags in ml_ar / ml_var:
#   (1) centres are the person means of the lags over the training rows only
#   (2) predictions use the training centre: intercept + slope * (lag - centre)
#   (3) no leakage: changing a person's data after an origin leaves that origin's forecast unchanged
#   (4) between = TRUE adds the person means as fixed effects and predicts finitely
#   (5) invalid settings and empty training windows fail loudly

library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))

set.seed(11)

make_person <- function(id, n = 40) {
  mu <- rnorm(2, 0.5, 0.1)
  Y <- matrix(NA_real_, nrow = n, ncol = 2, dimnames = list(NULL, c("x1", "x2")))
  Y[1, ] <- mu
  for (t in 2:n) Y[t, ] <- mu + 0.4 * (Y[t - 1, ] - mu) + rnorm(2, 0, 0.1)
  Ylag <- matrix(NA_real_, nrow = n, ncol = 2, dimnames = dimnames(Y))
  Ylag[2:n, ] <- Y[1:(n - 1), ]
  list(id = id, Y = Y, Ylag = Ylag, time = seq_len(n), valid = c(FALSE, rep(TRUE, n - 1)))
}
persons <- lapply(sprintf("p%02d", 1:20), make_person)
spec <- list(center_lag = "person", between = FALSE)

# ---------- (1) centres from the training rows ----------
train_rows <- 2:25
train <- lapply(persons, subset_modeldata, tps = train_rows)
fitted <- ml_ar_fit(train, spec)
expected <- colMeans(persons[[3]]$Ylag[train_rows, ])
stopifnot(isTRUE(all.equal(fitted$centres["p03", ], expected)),
          !isTRUE(all.equal(expected, colMeans(persons[[3]]$Ylag[-1, ]))))
cat("centres equal training-window means: PASS\n")

# ---------- (2) prediction = person intercept + person slope * (lag - centre) ----------
test <- subset_modeldata(persons[[3]], 26:30)
yhat <- ml_ar_predict(fitted, test)
for (v in c("x1", "x2")) {
  b <- coef(fitted$models[[v]])$id["p03", ]
  by_hand <- b[["(Intercept)"]] + b[[paste0(v, "_lag")]] * (test$Ylag[, v] - fitted$centres["p03", v])
  stopifnot(isTRUE(all.equal(unname(yhat[, v]), unname(by_hand))))
}
cat("predictions centre the test lag on the training mean: PASS\n")

# ---------- (3) no leakage through the centres ----------
cv <- list(test_window = 5, warmup = 10, refit_per_origin = TRUE)
origin <- make_folds(persons[[1]], cv)[[1]]$test
perturbed <- persons
later <- (origin + 1):40
perturbed[[1]]$Y[later, ] <- perturbed[[1]]$Y[later, ] + 0.3
perturbed[[1]]$Ylag[later, ] <- perturbed[[1]]$Ylag[later, ] + 0.3
for (m in c("ml_ar", "ml_var")) {
  at_origin <- function(ps) {
    out <- crossval_model(ps, model_registry[[m]], cv, spec = spec)
    out[out$set == "oos" & out$id == "p01" & out$t == origin, "yhat"]
  }
  stopifnot(identical(at_origin(persons), at_origin(perturbed)))
}
cat("forecast at an origin ignores later data: PASS\n")

# ---------- (4) between = TRUE ----------
spec_between <- list(center_lag = "person", between = TRUE)
fit_ar_b <- ml_ar_fit(train, spec_between)
fit_var_b <- ml_var_fit(train, spec_between)
stopifnot(identical(names(lme4::fixef(fit_ar_b$models$x1)), c("(Intercept)", "x1_lag", "x1_lagmean")),
          all(c("x1_lagmean", "x2_lagmean") %in% names(lme4::fixef(fit_var_b$models$x1))),
          all(is.finite(ml_ar_predict(fit_ar_b, test))),
          all(is.finite(ml_var_predict(fit_var_b, test))))
cat("between = TRUE adds the person means: PASS\n")

# ---------- (5) failures ----------
fails <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
stopifnot(fails(ml_ar_fit(train, list(center_lag = "none", between = TRUE))),
          fails(ml_ar_fit(train, list(center_lag = "grand"))))
empty <- train
empty[[1]] <- subset_modeldata(persons[[1]], integer(0))
stopifnot(fails(ml_ar_fit(empty, spec)))
stopifnot(is.null(ml_ar_fit(train, list(center_lag = "none"))$centres))
cat("invalid settings and empty training windows error: PASS\n")
