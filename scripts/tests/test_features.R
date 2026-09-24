library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "metrics.R"))
source(here::here("scripts", "engine", "run_dataset.R"))
source(here::here("scripts", "engine", "features.R"))

expect_error <- function(expr, pattern) {
  msg <- tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  stopifnot(!is.null(msg), grepl(pattern, msg))
}

set.seed(cfg$seed)
mock <- make_mock_openesm(seed = 1)
interim <- preprocess_dataset(mock$data, mock$meta, cfg, dataset_id = "mock01")
cfg_small <- cfg
cfg_small$active_models <- c("mean", "ar", "ml_ar")
result <- suppressMessages(run_dataset(interim, cfg_small))

f <- dataset_features("mock01", interim, result, cfg$cv)

# 1. every table is unique on its key
for (name in names(feature_keys)) {
  stopifnot(anyDuplicated(f[[name]][feature_keys[[name]]]) == 0)
}
cat("feature tables unique on their keys: PASS\n")

# 2. OOS t are valid source rows of the person's series
for (p in interim$persons) {
  t_oos <- unique(f$oos$t[f$oos$id == as.character(p$id)])
  stopifnot(all(t_oos >= 1), all(t_oos <= nrow(p$Y)), all(p$valid[t_oos]))
}
cat("OOS t are valid source rows: PASS\n")

# 3. all models agree on y at each OOS key, and every key has all models
by_key <- aggregate(y ~ id + variable + t, data = f$oos,
                    FUN = function(y) c(n = length(y), distinct = length(unique(y))))
stopifnot(all(by_key$y[, "n"] == length(cfg_small$active_models)),
          all(by_key$y[, "distinct"] == 1))
cat("models agree on y at each OOS key: PASS\n")

# 4. item_error rmse reproduces the metrics_var RMSE
mv <- result$metrics_var[result$metrics_var$set == "oos", ]
mv$rmse_metrics <- sqrt(mv$ss_res / mv$n)
cmp <- merge(f$item_error, mv, by = c("id", "variable", "model"))
stopifnot(nrow(cmp) == nrow(f$item_error), all(abs(cmp$rmse - cmp$rmse_metrics) < 1e-10))
cat("item_error rmse equals metrics_var RMSE: PASS\n")

# 5. duplicated prediction keys are rejected
result_dup <- result
first_oos <- which(result$oos$set == "oos")[1]
result_dup$oos <- rbind(result$oos, result$oos[first_oos, ])
expect_error(dataset_features("mock01", interim, result_dup, cfg$cv), "duplicated OOS prediction")
cat("duplicated prediction keys rejected: PASS\n")

# 6. a result built under different cv settings is rejected
cv_other <- cfg$cv
cv_other$test_window <- 5
expect_error(dataset_features("mock01", interim, result, cv_other), "different cv settings")
cat("cv mismatch rejected: PASS\n")

# 7. a result without the timepoint index is rejected
result_old <- result
result_old$oos$t <- NULL
expect_error(dataset_features("mock01", interim, result_old, cfg$cv), "no timepoint index")
cat("result without t rejected: PASS\n")

# 8. failed models are rejected rather than silently omitted
result_failed <- result
result_failed$meta$model_failures["ar"] <- TRUE
expect_error(dataset_features("mock01", interim, result_failed, cfg$cv),
             "models failed during fitting")
cat("failed models rejected: PASS\n")

# 9. empty OOS output is rejected
result_no_oos <- result
result_no_oos$oos <- result_no_oos$oos[result_no_oos$oos$set != "oos", , drop = FALSE]
expect_error(dataset_features("mock01", interim, result_no_oos, cfg$cv),
             "no OOS predictions")
cat("empty OOS output rejected: PASS\n")

# 10. rho1_train uses observed lag pairs only (LOCF-imputed lags excluded)
{
  set.seed(2)
  y <- as.vector(stats::arima.sim(list(ar = 0.5), n = 30))
  y[10] <- NA
  Y <- matrix(y, ncol = 1, dimnames = list(NULL, "x1"))
  Ylag <- matrix(c(NA, y[-30]), ncol = 1, dimnames = list(NULL, "x1"))
  Ylag[11, 1] <- y[9]  # LOCF fill for the missing y[10]
  p <- list(id = "u1", Y = Y, Ylag = Ylag, time = seq_len(30),
            valid = !is.na(Y[, 1]) & !is.na(Ylag[, 1]))
  cv_unit <- list(test_window = 5, warmup = 5, refit_per_origin = TRUE)
  pf <- person_features(p, cv_unit, "unit")
  # first origin is row 26; pairs t = 2..25 minus t = 10 (y missing) and t = 11 (imputed lag)
  expected_t <- setdiff(2:25, c(10, 11))
  stopifnot(pf$item$n_pairs_train == length(expected_t),
            abs(pf$item$rho1_train - cor(y[expected_t], y[expected_t - 1])) < 1e-12,
            pf$person$n_train_first == 23)  # valid rows 2..25 except 10; includes imputed row 11
  cat("rho1_train excludes imputed lags: PASS\n")
}

cat("all feature tests passed\n")
