# Unit tests for ml_var_fit / ml_var_predict interface behavior.
# Separate from test_ml_recovery.R (parameter recovery); these test:
#   (1) both re_corr branches produce finite predictions
#   (2) ml_var_predict aligns Ylag by column name, not position

library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "models.R"))

set.seed(7)

# minimal trivariate persons: 15 persons x 40 obs, enough for lmer to run
make_tri_person <- function(id, n = 40) {
  Y <- matrix(rnorm(n * 3), nrow = n, ncol = 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  Ylag <- matrix(NA_real_, nrow = n, ncol = 3,
                 dimnames = list(NULL, c("x1", "x2", "x3")))
  Ylag[2:n, ] <- Y[1:(n - 1), , drop = FALSE]
  list(id = id, Y = Y, Ylag = Ylag, time = seq_len(n),
       valid = c(FALSE, rep(TRUE, n - 1)))
}

persons <- lapply(paste0("p", seq_len(15)), make_tri_person)

# ---------- (1a) re_corr = TRUE branch ----------
fitted_corr <- ml_var_fit(persons, spec = list(re_corr = TRUE))
stopifnot(length(fitted_corr$models) == 3)
yhat_corr <- ml_var_predict(fitted_corr, persons[[1]])
stopifnot(is.matrix(yhat_corr), all(is.finite(yhat_corr[persons[[1]]$valid, ])))
cat("re_corr = TRUE  branch: PASS\n")

# ---------- (1b) re_corr = FALSE branch ----------
fitted_uncorr <- ml_var_fit(persons, spec = list(re_corr = FALSE))
stopifnot(length(fitted_uncorr$models) == 3)
yhat_uncorr <- ml_var_predict(fitted_uncorr, persons[[1]])
stopifnot(is.matrix(yhat_uncorr), all(is.finite(yhat_uncorr[persons[[1]]$valid, ])))
cat("re_corr = FALSE branch: PASS\n")

# ---------- (2) permuted Ylag columns produce identical predictions ----------
p_test <- persons[[1]]

# permute Ylag column order: x3, x1, x2
p_permuted <- p_test
p_permuted$Ylag <- p_permuted$Ylag[, c("x3", "x1", "x2"), drop = FALSE]

yhat_orig    <- ml_var_predict(fitted_corr, p_test)
yhat_permuted <- ml_var_predict(fitted_corr, p_permuted)
stopifnot(isTRUE(all.equal(yhat_orig, yhat_permuted)))
cat("permuted Ylag columns aligned by name: PASS\n")
