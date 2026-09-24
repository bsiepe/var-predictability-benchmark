#------------- Features -------------
# Descriptive series features and time-indexed OOS predictions for the report
# Operates on one interim object and its result object, does not refit models
# results can then be appended to the prediction

# grain of each table in the feature cache
feature_keys <- list(
  item = c("dataset_id", "id", "variable"),
  person = c("dataset_id", "id"),
  item_error = c("dataset_id", "id", "variable", "model"),
  series = c("dataset_id", "id", "variable", "t"),
  oos = c("dataset_id", "id", "variable", "model", "t")
)

# simple check that stops with a message if the condition is not TRUE
.check <- function(ok, where, msg) {
  if (!isTRUE(ok)) stop(where, ": ", msg, call. = FALSE)
}

# check that a table has no duplicated keys, stops with a message if it does
.assert_unique <- function(tab, key, where, name) {
  .check(anyDuplicated(tab[key]) == 0, where,
         sprintf("duplicated %s keys (%s)", name, paste(key, collapse = ", ")))
}

# NA instead of a warning when a series is constant or too short
cor_or_na <- function(x, y) {
  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) return(NA_real_)
  stats::cor(x, y)
}

# item-level features, person-level summaries and the observed series for one person
person_features <- function(person_data, cv, ds_id) {
  Y <- person_data$Y
  Ylag <- person_data$Ylag
  tn <- nrow(Y)
  items <- colnames(Y)
  id <- as.character(person_data$id)
  where <- paste0(ds_id, "/", id)

  # lag_ok is not stored on the person, so it is read off Ylag: where the raw previous value
  # is observed, Ylag must equal it wherever the lag was allowed
  Yprev <- rbind(matrix(NA_real_, 1, ncol(Y)), Y[-tn, , drop = FALSE])
  both <- !is.na(Yprev) & !is.na(Ylag)
  .check(all(Ylag[both] == Yprev[both]), where, "Ylag differs from the observed previous row")

  folds <- make_folds(person_data, cv)
  # training window of the first OOS fold
  # later folds train on up to test_window - 1 more rows
  first_origin <- if (length(folds) > 0) min(folds[[1]]$test) else tn + 1L
  in_train <- seq_len(tn) < first_origin

  # training-window pairs (Y[t, i], Y[t - 1, j]) with both values observed and the lag allowed.
  # excludes LOCF-imputed lags, so correlations describe the observed series
  observed_lag_pairs <- function(i, j) {
    in_train & !is.na(Y[, i]) & !is.na(Yprev[, j]) & !is.na(Ylag[, j])
  }

  item <- dplyr::bind_rows(lapply(seq_along(items), function(v) {
    y <- Y[, v]
    obs <- !is.na(y)
    ok <- observed_lag_pairs(v, v)
    dplyr::tibble(
      id = id,
      variable = items[v],
      n_obs = sum(obs),
      sd_obs = if (sum(obs) > 1) stats::sd(y[obs]) else NA_real_,
      # items are range01-rescaled to their scale limits, so bounds are 0 and 1
      prop_bound = mean(abs(y[obs]) < 1e-8 | abs(y[obs] - 1) < 1e-8),
      rho1_train = cor_or_na(y[ok], Yprev[ok, v]),
      n_pairs_train = sum(ok)
    )
  }))

  # create cross-table of all item pairs
  pairs <- expand.grid(i = seq_along(items), j = seq_along(items))
  pairs <- pairs[pairs$i != pairs$j, ]
  cross <- mapply(function(i, j) {
    ok <- observed_lag_pairs(i, j)
    cor_or_na(Y[ok, i], Yprev[ok, j])
  }, pairs$i, pairs$j)
  # mean absolute cross-lag correlation over all item pairs
  cross_lag_train <- if (any(!is.na(cross))) mean(abs(cross), na.rm = TRUE) else NA_real_

  n_train_first <- if (length(folds) > 0) length(folds[[1]]$train) else NA_integer_

  # person means weight items equally, over items where the feature is defined
  person <- dplyr::tibble(
    id = id,
    p = length(items),
    n_valid = sum(person_data$valid),
    n_train_first = n_train_first,
    obs_per_var_eq_par = n_train_first / (length(items) + 1),
    cross_lag_train = cross_lag_train,
    sd_obs_mean = mean(item$sd_obs, na.rm = TRUE),
    prop_bound_mean = mean(item$prop_bound, na.rm = TRUE),
    rho1_train_mean = mean(item$rho1_train, na.rm = TRUE)
  )

  series <- dplyr::tibble(
    id = id,
    variable = rep(items, each = tn),
    t = rep(seq_len(tn), times = length(items)),
    y = as.vector(Y),
    valid = rep(person_data$valid, times = length(items))
  )

  list(item = item, person = person, series = series)
}

# checks that predictions, error summaries, cached metrics and the observed series agree
# important bc misalignment due to filtering or reshaping can be hard to detect in the report
validate_result_alignment <- function(ds_id, oos, item_error, metrics_var, series) {
  mv <- metrics_var |>
    dplyr::filter(set == "oos") |>
    dplyr::transmute(dataset_id = ds_id, id = as.character(id), variable, model, n,
                     rmse_metrics = sqrt(ss_res / n))
  .assert_unique(mv, feature_keys$item_error, ds_id, "metrics_var")
  joined <- dplyr::inner_join(item_error, mv, by = feature_keys$item_error)
  .check(nrow(joined) == nrow(item_error) && nrow(joined) == nrow(mv), ds_id,
         "item_error and metrics_var cover different person x item x model rows")
  .check(all(joined$n_e == joined$n), ds_id, "n_e differs from metrics_var n")
  # rmse is bounded in [0, 1] and both sides use the same rows, so an absolute tolerance suffices
  .check(all(abs(joined$rmse - joined$rmse_metrics) < 1e-10), ds_id,
         "item_error rmse does not reproduce the metrics_var RMSE")
  .check(all(abs(item_error$mse - (item_error$me^2 + item_error$var_e)) < 1e-12), ds_id,
         "mse != me^2 + var_e")

  # matching every row to the series also means all models agree on y at each key
  on_series <- dplyr::inner_join(oos, series, by = feature_keys$series,
                                 suffix = c("", "_series"))
  .check(nrow(on_series) == nrow(oos), ds_id, "OOS rows with t outside the observed series")
  .check(all(on_series$y == on_series$y_series), ds_id, "OOS y differs from the observed series")
  .check(all(on_series$valid), ds_id, "OOS predictions at timepoints that are not valid")
  invisible(TRUE)
}

# all feature tables for one dataset; stops on any inconsistency
dataset_features <- function(ds_id, interim, result, cv) {
  .check(identical(result$meta$settings$cv, cv), ds_id,
         "result was built with different cv settings than the current config")
  .check(identical(result$meta$settings$preprocess, interim$settings), ds_id,
         "result was built from different preprocessing settings than the interim file")
  .check("t" %in% names(result$oos), ds_id,
         "result has no timepoint index; refit with the current engine")
    .check("model_failures" %in% names(result$meta), ds_id,
      "result has no model failure metadata; refit with the current engine")
    failed_models <- names(result$meta$model_failures)[result$meta$model_failures]
    .check(length(failed_models) == 0, ds_id,
      paste("models failed during fitting:", paste(failed_models, collapse = ", ")))
  person_ids <- vapply(interim$persons, function(p) as.character(p$id), character(1))
  .check(setequal(person_ids, names(result$meta$n_valid)), ds_id,
         "persons differ between interim file and result")

  per_person <- lapply(interim$persons, person_features, cv = cv, ds_id = ds_id)
  # bind_rows() would drop the dataset_id column, so we add it after the bind
  bind_table <- function(name) {
    dplyr::mutate(dplyr::bind_rows(lapply(per_person, `[[`, name)),
                  dataset_id = ds_id, .before = 1)
  }
  item <- bind_table("item")
  person <- bind_table("person")
  series <- bind_table("series")
  .check(all(person$n_valid == result$meta$n_valid[person$id]), ds_id,
         "n_valid differs from the result metadata")

  # OOS predictions are already filtered to the test folds, but they may contain rows with NA y or yhat
  oos <- result$oos |>
    dplyr::filter(set == "oos") |>
    dplyr::mutate(dataset_id = ds_id, id = as.character(id), variable, model, t, y, yhat, .keep = "none")
  .check(nrow(oos) > 0, ds_id, "result contains no OOS predictions")
  # duplicates would inflate n and ss_res alike, so the metric comparison could not catch them
  .assert_unique(oos, feature_keys$oos, ds_id, "OOS prediction")

  # exactly the metric row set: metrics.R filters to finite y and yhat
  item_error <- oos |>
    dplyr::filter(is.finite(yhat), is.finite(y)) |>
    dplyr::mutate(e = y - yhat) |>
    dplyr::summarise(n_e = dplyr::n(), me = mean(e), var_e = mean((e - mean(e))^2),
                     mse = mean(e^2), .by = dplyr::all_of(feature_keys$item_error)) |>
    dplyr::mutate(rmse = sqrt(mse))

  validate_result_alignment(ds_id, oos, item_error, result$metrics_var, series)

  tabs <- list(item = item, person = person, item_error = item_error,
               series = series, oos = oos)
  for (name in names(tabs)) .assert_unique(tabs[[name]], feature_keys[[name]], ds_id, name)
  c(tabs, list(settings = interim$settings))
}
