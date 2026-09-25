library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "models.R"))

# collect warnings raised by expr and return its value alongside them
with_warnings <- function(expr) {
  warns <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warns)
}

set.seed(1)
n <- 30
make_train <- function(Y) {
  Ylag <- rbind(NA, Y[-nrow(Y), , drop = FALSE])
  list(id = "p1", Y = Y, Ylag = Ylag, time = seq_len(nrow(Y)),
       valid = c(FALSE, rep(TRUE, nrow(Y) - 1)))
}
Y_full <- matrix(runif(n * 3), n, 3, dimnames = list(NULL, c("x1", "x2", "x3")))

# 1. full-rank input: same coefficients as plain lm.fit, no warning
train <- make_train(Y_full)
rows <- train$valid
res <- with_warnings(var_fit(train, NULL))
ref <- stats::lm.fit(cbind(1, train$Ylag[rows, ]), train$Y[rows, ])$coefficients
stopifnot(length(res$warnings) == 0, isTRUE(all.equal(res$value$coefs, ref)))
res_ar <- with_warnings(ar_fit(train, NULL))
stopifnot(length(res_ar$warnings) == 0, all(is.finite(res_ar$value$coefs)))
cat("full rank: unchanged coefficients, no warning: PASS\n")

# 2. one constant lagged item: finite coefficients and predictions, exactly one warning,
#    predictions equal least squares on the design without the constant column
Y_const <- Y_full
Y_const[, "x2"] <- 0.5
train <- make_train(Y_const)
res <- with_warnings(var_fit(train, NULL))
stopifnot(length(res$warnings) == 1, grepl("rank-deficient OLS fit", res$warnings),
          all(is.finite(res$value$coefs)))
Yhat <- var_predict(res$value, train)
X_reduced <- cbind(1, train$Ylag[rows, c("x1", "x3")])
ref_fit <- stats::lm.fit(X_reduced, train$Y[rows, ])
stopifnot(all(is.finite(Yhat[rows, ])),
          isTRUE(all.equal(unname(Yhat[rows, ]), unname(X_reduced %*% ref_fit$coefficients))))
res_ar <- with_warnings(ar_fit(train, NULL))
stopifnot(length(res_ar$warnings) == 1, all(is.finite(ar_predict(res_ar$value, train)[rows, ])))
cat("constant lagged item: aliased set to 0, one warning per fit call: PASS\n")

# 3. more coefficients than observations: finite predictions, one warning
Y_wide <- matrix(runif(6 * 8), 6, 8, dimnames = list(NULL, paste0("x", 1:8)))
train <- make_train(Y_wide)
res <- with_warnings(var_fit(train, NULL))
stopifnot(length(res$warnings) == 1, all(is.finite(var_predict(res$value, train)[train$valid, ])))
cat("p + 1 > n: finite predictions, one warning: PASS\n")

# 4. lme4 warnings from ml_ar_fit and ml_var_fit are no longer muffled
persons <- lapply(1:5, function(i) {
  p <- make_train(matrix(runif(n * 2), n, 2, dimnames = list(NULL, c("x1", "x2"))))
  p$id <- paste0("p", i)
  p
})
suppressMessages(trace(lme4::lmer, exit = quote(warning("Model failed to converge (test)")),
                       print = FALSE))
res_ml_ar <- with_warnings(suppressMessages(ml_ar_fit(persons, NULL)))
res_ml_var <- with_warnings(suppressMessages(ml_var_fit(persons, list(re_corr = FALSE))))
suppressMessages(untrace(lme4::lmer))
stopifnot(sum(grepl("failed to converge", res_ml_ar$warnings)) == 2,
          sum(grepl("failed to converge", res_ml_var$warnings)) == 2)
cat("ml_ar and ml_var pass lme4 warnings on: PASS\n")

cat("all OLS model tests passed\n")
