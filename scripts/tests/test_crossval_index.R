library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))

# checks that the timepoint index t aligns every prediction with the person's series,
# for a person-level (mean) and a dataset-level (ml_ar) model

set.seed(cfg$seed)
mock <- make_mock_openesm(seed = 1)
interim <- preprocess_dataset(mock$data, mock$meta, cfg, dataset_id = "mock01")
persons <- interim$persons

preds <- lapply(c("mean", "ml_ar"), function(m) {
  cbind(model = m, crossval_model(persons, model_registry[[m]], cfg$cv))
})
names(preds) <- c("mean", "ml_ar")

for (m in names(preds)) {
  out <- preds[[m]]

  for (p in persons) {
    folds <- make_folds(p, cfg$cv)
    test_tps <- unlist(lapply(folds, function(f) f$test))

    for (v in colnames(p$Y)) {
      rows_oos <- out[out$id == p$id & out$variable == v & out$set == "oos", ]
      rows_in <- out[out$id == p$id & out$variable == v & out$set == "in", ]

      # 1. OOS t equals the fold test indices, in fold order
      stopifnot(identical(as.integer(rows_oos$t), as.integer(test_tps)))
      # 2. in-sample t equals the valid timepoints
      stopifnot(identical(as.integer(rows_in$t), which(p$valid)))
      # 4. one OOS row per fold
      stopifnot(nrow(rows_oos) == length(folds))
      # observed values at t match the series
      stopifnot(isTRUE(all.equal(rows_oos$y, unname(p$Y[rows_oos$t, v]))))
    }
  }

  # 3. key uniqueness within model
  stopifnot(!anyDuplicated(out[, c("id", "variable", "set", "t")]))
  cat(sprintf("[%s] t matches folds and valid rows, keys unique: PASS\n", m))
}

# 5. y agrees across models at the same key
key <- c("id", "variable", "set", "t")
both <- merge(preds$mean[, c(key, "y")], preds$ml_ar[, c(key, "y")], by = key,
              suffixes = c("_mean", "_ml_ar"))
stopifnot(nrow(both) == nrow(preds$mean), isTRUE(all.equal(both$y_mean, both$y_ml_ar)))
cat("y agrees across models at the same key: PASS\n")

# 6. mean model: yhat at origin o is the mean of valid Y before o
oos_mean <- preds$mean[preds$mean$set == "oos", ]
for (p in persons) {
  valid_tps <- which(p$valid)
  for (o in unique(oos_mean$t[oos_mean$id == p$id])) {
    expected <- colMeans(p$Y[valid_tps[valid_tps < o], , drop = FALSE])
    got <- oos_mean[oos_mean$id == p$id & oos_mean$t == o, ]
    stopifnot(isTRUE(all.equal(got$yhat, unname(expected[got$variable]))))
  }
}
cat("mean model: yhat equals expanding training mean at each origin: PASS\n")

cat("all crossval index tests passed\n")
