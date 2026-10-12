library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "relative.R"))

# pinned rather than read from cfg, so a robustness setting there does not break the tolerances
spec <- list(ebic_gamma = 0, penalize_ar = TRUE, min_resid_df = 1, lambda_min_ratio = 0.05)
lasso <- model_registry$lasso_var
spec_of <- function(m) if (m == "lasso_var") spec else cfg[[paste0(m, ".spec")]]

# one person in the engine's format, simulated from a VAR(1) with Phi[j, k] = effect of j on k
simulate_person <- function(n, Phi, id = "p1", burn_in = 50) {
  p <- ncol(Phi)
  Y <- matrix(0, n + burn_in, p)
  for (t in 2:nrow(Y)) Y[t, ] <- Y[t - 1, ] %*% Phi + rnorm(p)
  Y <- Y[-seq_len(burn_in), , drop = FALSE]
  colnames(Y) <- paste0("x", seq_len(p))
  list(id = id, Y = Y, Ylag = rbind(NA, Y[-n, , drop = FALSE]), time = seq_len(n),
       valid = c(FALSE, rep(TRUE, n - 1)))
}

fit_predict <- function(model, person, spec) {
  full <- subset_modeldata(person, which(person$valid))
  model$predict(model$fit(full, spec), full)
}

oos_predictions <- function(model, person, spec) {
  out <- crossval_person(person, model, cfg$cv, spec)
  out[out$set == "oos", ]
}

prediction_at <- function(out, t) out$yhat[out$t == t]

# collect warnings raised by expr and return its value alongside them
with_warnings <- function(expr) {
  warns <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warns)
}

# 1. leakage: the prediction at origin o must not change when the target and everything after it
#    is overwritten. covers every person-level model, so a shared fold bug is caught too
set.seed(11)
person <- simulate_person(60, diag(0.6, 4))
person_models <- names(Filter(function(m) m$level == "person", model_registry))
origins <- unlist(lapply(make_folds(person, cfg$cv), `[[`, "test"))
n_rows <- nrow(person$Y)
for (m in person_models) {
  base <- oos_predictions(model_registry[[m]], person, spec_of(m))
  for (o in origins) {
    poisoned <- person
    poisoned$Y[o:n_rows, ] <- 1e6
    poisoned$Ylag[seq_len(n_rows) > o, ] <- 1e6
    out <- oos_predictions(model_registry[[m]], poisoned, spec_of(m))
    stopifnot(identical(prediction_at(out, o), prediction_at(base, o)))
  }
}
# the check is not vacuous: the training window and the current lag do reach the predictions
o <- origins[1]
poisoned_train <- person
poisoned_train$Y[o - 1, ] <- 1e6
poisoned_lag <- person
poisoned_lag$Ylag[o, ] <- 1e6
for (m in setdiff(person_models, "locf")) {
  base <- oos_predictions(model_registry[[m]], person, spec_of(m))
  out <- oos_predictions(model_registry[[m]], poisoned_train, spec_of(m))
  stopifnot(!identical(prediction_at(out, o), prediction_at(base, o)))
}
for (m in c("locf", "ar", "var", "lasso_var")) {
  base <- oos_predictions(model_registry[[m]], person, spec_of(m))
  out <- oos_predictions(model_registry[[m]], poisoned_lag, spec_of(m))
  stopifnot(!identical(prediction_at(out, o), prediction_at(base, o)))
}
cat("no leakage from the target or later rows, for all person-level models: PASS\n")

# 2. nesting: a prohibitive support penalty leaves the empty model, which is the mean;
#    with the own lag unpenalized it leaves the own lag only, which is the OLS AR(1)
set.seed(12)
person <- simulate_person(40, diag(0.5, 4))
prohibitive <- modifyList(spec, list(ebic_gamma = 1e6))
# the first path point is the smallest admissible lambda only if nothing else passes the cap
empty_fit <- with_warnings(fit_predict(lasso, person, prohibitive))
stopifnot(length(empty_fit$warnings) == 0)
diff_mean <- max(abs(empty_fit$value - fit_predict(model_registry$mean, person, NULL)))
diff_ar <- max(abs(fit_predict(lasso, person, modifyList(prohibitive, list(penalize_ar = FALSE))) -
                     fit_predict(model_registry$ar, person, NULL)))
cat(sprintf("max |lasso - mean| = %.1e, max |lasso - ar| = %.1e\n", diff_mean, diff_ar))
stopifnot(diff_mean < 1e-10, diff_ar < 1e-8)
cat("prohibitive penalty reduces to mean and to AR(1): PASS\n")

# 3. in-sample ordering per person and item: OLS minimises the residual sum of squares, and every
#    LASSO solution has a residual sum of squares no larger than the intercept-only fit
mock <- make_mock_openesm(seed = 1)
interim <- suppressMessages(preprocess_dataset(mock$data, mock$meta, cfg, dataset_id = "mock01"))
in_sample_ss <- function(m) {
  unlist(lapply(interim$persons, function(p) {
    Y <- subset_modeldata(p, which(p$valid))$Y
    colSums((fit_predict(model_registry[[m]], p, spec_of(m)) - Y)^2)
  }))
}
ss_var <- in_sample_ss("var")
ss_lasso <- in_sample_ss("lasso_var")
ss_mean <- in_sample_ss("mean")
tolerance <- 1e-6 * ss_mean
stopifnot(all(ss_var <= ss_lasso + tolerance), all(ss_lasso <= ss_mean + tolerance))
cat(sprintf("in-sample var <= lasso_var <= mean for all %d person-items: PASS\n", length(ss_mean)))

# 4. large n: all entries of a dense, asymmetric Phi are selected, close to OLS and the truth.
#    the truth check fails for a transposed coefficient matrix (Phi[1, 2] = -0.2 vs Phi[2, 1] = 0.2)
set.seed(42)
Phi <- matrix(c(0.50, 0.20, -0.15,
                -0.20, 0.40, 0.10,
                0.10, -0.10, 0.30), nrow = 3)
person <- simulate_person(2000, Phi)
full <- subset_modeldata(person, which(person$valid))
dense_fit <- with_warnings(lasso_var_fit(full, spec))
coefs_lasso <- dense_fit$value$coefs
coefs_ols <- var_fit(full, NULL)$coefs
diff_ols <- max(abs(coefs_lasso - coefs_ols))
diff_truth <- max(abs(coefs_lasso[-1, ] - Phi))
cat(sprintf("n = 2000: max |lasso - ols| = %.3f, max |lasso - Phi| = %.3f\n", diff_ols, diff_truth))
stopifnot(all(coefs_lasso[-1, ] != 0), diff_ols < 0.05, diff_truth < 0.1)
# with this much data BIC keeps improving down to the end of the shortened path: one warning per
# fit call, not one per equation
stopifnot(length(dense_fit$warnings) == 1,
          grepl("lasso selection at the smallest admissible lambda", dense_fit$warnings))
cat("large-n recovery and orientation, boundary warned once: PASS\n")

# 5. sparse truth, short series: regularisation must beat OLS out of sample.
#    expected OLS excess ~ (p + 1) / n = 7 / 35 of the innovation variance; 0.97 leaves room
#    for noise. paired differences are averaged per person, as errors within a person correlate
set.seed(15)
persons <- lapply(seq_len(200), function(i) simulate_person(40, diag(0.4, 6), id = paste0("p", i)))
oos_errors <- function(m) {
  out <- crossval_model(persons, model_registry[[m]], cfg$cv, spec_of(m))
  out[out$set == "oos", ]
}
oos_var <- oos_errors("var")
oos_lasso <- oos_errors("lasso_var")
stopifnot(identical(oos_var[c("id", "variable", "t")], oos_lasso[c("id", "variable", "t")]))
err_var <- (oos_var$yhat - oos_var$y)^2
err_lasso <- (oos_lasso$yhat - oos_lasso$y)^2
mse_ratio <- mean(err_lasso) / mean(err_var)
person_diff <- tapply(err_lasso - err_var, oos_var$id, mean)
diff_se <- sd(person_diff) / sqrt(length(person_diff))
cat(sprintf("sparse truth: MSE ratio lasso / var = %.3f, paired difference = %.3f (SE %.3f)\n",
            mse_ratio, mean(person_diff), diff_se))
stopifnot(mse_ratio < 0.97, mean(person_diff) < -2 * diff_se)
cat("sparse truth, short series: lasso_var beats var OOS: PASS\n")

# 6. a larger gamma never selects more slopes, with and without a penalized own lag
set.seed(16)
for (i in seq_len(100)) {
  Phi <- diag(0.4, 8)
  Phi[sample(which(Phi == 0), 3)] <- 0.15
  person <- simulate_person(25, Phi)
  full <- subset_modeldata(person, which(person$valid))
  for (penalize_ar in c(TRUE, FALSE)) {
    n_slopes <- vapply(c(0, 0.5, 2), function(gamma) {
      s <- modifyList(spec, list(ebic_gamma = gamma, penalize_ar = penalize_ar))
      sum(lasso_var_fit(full, s)$coefs[-1, ] != 0)
    }, numeric(1))
    stopifnot(all(diff(n_slopes) <= 0))
  }
}
cat("selected slopes non-increasing in gamma: PASS\n")

# 7. the returned coefficients are the path column at the constrained EBIC minimum, recomputed
#    independently: rss from predict() instead of deviance(), slopes counted from beta, and the
#    support penalty written out as in graphicalVAR's Rothmana.R, (pdO + pdB) * 4 * gamma * log(2 * nY)
set.seed(17)
n_vars <- 12
for (n in c(10, 25, 60)) {
  for (min_resid_df in c(1, 5)) {
    for (gamma in c(0, 0.5)) {
      X <- matrix(rnorm(n * n_vars), n, n_vars)
      y <- 0.5 * X[, 1] + rnorm(n)
      s <- modifyList(spec, list(min_resid_df = min_resid_df, ebic_gamma = gamma))
      equation <- .lasso_coefs(X, y, own = 1, s)
      fit <- glmnet::glmnet(X, y, lambda.min.ratio = s$lambda_min_ratio)
      rss <- colSums((y - stats::predict(fit, newx = X))^2)
      n_slopes <- colSums(as.matrix(fit$beta) != 0)
      ebic <- n * log(rss / n) + n_slopes * log(n) + n_slopes * 4 * gamma * log(2 * n_vars)
      ebic[n - (n_slopes + 1) < min_resid_df] <- Inf
      best <- which.min(ebic)
      last_admissible <- max(which(is.finite(ebic)))
      stopifnot(max(abs(equation$coefs - c(fit$a0[best], fit$beta[, best]))) < 1e-12,
                n - (sum(equation$coefs[-1] != 0) + 1) >= min_resid_df,
                identical(equation$at_boundary, best == last_admissible))
    }
  }
}
cat("selection is the constrained EBIC argmin: PASS\n")

# 8. exact fits below the cap. (i) a sparse exact relation is found;
#    (ii) with the own lag unpenalized, rss is ~0 at the first path point
set.seed(18)
X <- matrix(runif(30 * 4), 30, 4)
y <- 0.2 + 0.5 * X[, 1]
coefs <- .lasso_coefs(X, y, own = 2, spec)$coefs
max_error <- max(abs(cbind(1, X) %*% coefs - y))
cat(sprintf("exact sparse fit: max prediction error %.1e (sd(y) = %.3f), %d slopes\n",
            max_error, sd(y), sum(coefs[-1] != 0)))
stopifnot(all(is.finite(coefs)), max_error < 0.1 * sd(y), sum(coefs[-1] != 0) <= 2)
coefs <- .lasso_coefs(X, y, own = 1, modifyList(spec, list(penalize_ar = FALSE)))$coefs
stopifnot(max(abs(cbind(1, X) %*% coefs - y)) < 1e-8, max(abs(coefs[1:2] - c(0.2, 0.5))) < 1e-8)
cat("exact fits under the cap: PASS\n")

# 9. permuting items permutes the selected support exactly and the predictions up to glmnet's
#    convergence: coordinate descent visits the columns in another order and stops on squared
#    coefficient changes below thresh = 1e-7, so coefficients agree to about sqrt(1e-7)
set.seed(19)
Phi <- diag(0.4, 5)
Phi[1, 3] <- 0.3
Phi[4, 2] <- -0.3
person <- simulate_person(50, Phi)
perm <- c(3, 5, 1, 4, 2)
permuted <- person
permuted$Y <- person$Y[, perm]
permuted$Ylag <- person$Ylag[, perm]
full <- subset_modeldata(person, which(person$valid))
full_permuted <- subset_modeldata(permuted, which(permuted$valid))
for (penalize_ar in c(TRUE, FALSE)) {
  s <- modifyList(spec, list(penalize_ar = penalize_ar))
  coefs <- lasso_var_fit(full, s)$coefs[c(1, perm + 1), perm]
  coefs_permuted <- lasso_var_fit(full_permuted, s)$coefs
  stopifnot(identical(coefs != 0, coefs_permuted != 0),
            max(abs(fit_predict(lasso, person, s)[, perm] - fit_predict(lasso, permuted, s))) < 1e-3)
}
cat("item permutation equivariance: PASS\n")

# 10. edge cases
set.seed(20)
person <- simulate_person(30, diag(0.4, 3))
person$Y[, "x1"] <- 0.5
person$Ylag[-1, "x1"] <- 0.5
full <- subset_modeldata(person, which(person$valid))
coefs <- lasso_var_fit(full, spec)$coefs
stopifnot(identical(unname(coefs[, "x1"]), c(0.5, 0, 0, 0)))
# no lag varies but the response does: glmnet would stop, the fit falls back to the mean
full$Ylag[] <- 0.5
coefs <- lasso_var_fit(full, spec)$coefs
stopifnot(max(abs(coefs[1, ] - colMeans(full$Y))) < 1e-12, all(coefs[-1, ] == 0))
# fewer observations than items: finite predictions, cap respected
person <- simulate_person(11, diag(0.3, 12))
full <- subset_modeldata(person, which(person$valid))
coefs <- lasso_var_fit(full, spec)$coefs
n <- sum(person$valid)
stopifnot(all(is.finite(var_predict(list(coefs = coefs), full))),
          all(n - (colSums(coefs[-1, ] != 0) + 1) >= spec$min_resid_df))
# a single item errors loudly instead of failing silently
person <- simulate_person(30, matrix(0.4))
single_item <- tryCatch(lasso_var_fit(subset_modeldata(person, which(person$valid)), spec),
                        error = conditionMessage)
stopifnot(is.character(single_item), grepl("2 or more columns", single_item))
cat("edge cases (constant response, constant lags, n < p, single item): PASS\n")

# 11. empty-selection share in the collection step: 1 under a prohibitive penalty, below 1 at the
#     default. a failed lasso_var leaves no rows; a lasso_var row without a mean row stops
oos_mock <- function(lasso_spec) {
  # boundary warnings are tested above
  lasso_oos <- suppressWarnings(crossval_model(interim$persons, lasso, cfg$cv, lasso_spec))
  mean_oos <- crossval_model(interim$persons, model_registry$mean, cfg$cv)
  rbind(cbind(model = "lasso_var", lasso_oos), cbind(model = "mean", mean_oos)) |>
    transform(dataset_id = "mock01")
}
oos_default <- oos_mock(spec)
share_default <- lasso_empty_share(oos_default)
share_prohibitive <- lasso_empty_share(oos_mock(modifyList(spec, list(ebic_gamma = 1e6))))
print(share_default)
stopifnot(setequal(share_default$set, c("in", "oos")), all(share_default$share_empty < 1),
          all(share_prohibitive$share_empty == 1))
stopifnot(nrow(lasso_empty_share(oos_default[oos_default$model == "mean", ])) == 0)
orphan <- oos_default[!(oos_default$model == "mean" & oos_default$id == oos_default$id[1]), ]
stopifnot(inherits(tryCatch(lasso_empty_share(orphan), error = identity), "error"))
cat("empty-selection share: PASS\n")

cat("all lasso_var tests passed\n")
