#------------- Relative MSE -------------
# Accuracy of each model relative to the rolling mean model, from the cached item metrics,
# and helpers for the meta-regression moderators
# Not an engine dependency of the fits: changing this file only reruns the collection step

# per person x model x set:
#   rel_mse: geometric mean over items of MSE_model / MSE_mean
#   gm_mse: geometric mean over items of MSE_model
#   reason_na: why rel_mse is undefined ("missing", "zero_reference", "zero_model"), NA otherwise
# metrics needs dataset_id, id, model, variable, set, ss_res, n (one row per item)
relative_mse <- function(metrics, zero_tol = 1e-10) {
  item_key <- c("dataset_id", "id", "variable", "set")
  # an exact fit (e.g. ar on a constant item) leaves floating-point noise around 1e-35 instead
  # of 0. zero_tol (RMSE 1e-5 on the [0, 1] scale) treats it as zero. real errors are far larger
  # items without scored test rows count as missing, never as a division by zero
  safe_mse <- function(ss_res, n) dplyr::if_else(!is.na(n) & n > 0, ss_res / n, NA_real_)
  
  # calculate reference performance (mean model)
  ref <- metrics |>
    dplyr::filter(model == "mean") |>
    dplyr::mutate(n_ref = n, mse_ref = safe_mse(ss_res, n)) |>
    dplyr::select(dplyr::all_of(item_key), n_ref, mse_ref)
  n_items_ref <- dplyr::count(ref, dataset_id, id, set, name = "n_items_ref")

  item <- metrics |>
    dplyr::select(dplyr::all_of(item_key), model, ss_res, n) |>
    dplyr::left_join(ref, by = item_key) |>
    dplyr::mutate(mse = safe_mse(ss_res, n))

  # ratios are only meaningful if every model is scored on the same test rows as the reference
  mismatch <- dplyr::filter(item, n > 0, n_ref > 0, n != n_ref)
  if (nrow(mismatch) > 0) {
    stop(nrow(mismatch), " person x item x model rows scored on a different number of test rows ",
         "than the mean model, e.g. dataset ", mismatch$dataset_id[1], ", person ",
         mismatch$id[1], ", model ", mismatch$model[1], call. = FALSE)
  }

  item |>
  # create grouped summary of the item-level metrics, then join with the reference counts
    dplyr::summarise(
      n_items = dplyr::n(),
      missing = anyNA(mse) | anyNA(mse_ref),
      zero_ref = any(mse_ref < zero_tol, na.rm = TRUE),
      zero_model = any(mse < zero_tol, na.rm = TRUE),
      log_ratio = mean(log(mse / mse_ref)),
      log_mse = mean(log(mse)),
      .by = c(dataset_id, id, model, set)
    ) |>
    dplyr::left_join(n_items_ref, by = c("dataset_id", "id", "set")) |>
    dplyr::mutate(
      missing = missing | is.na(n_items_ref) | n_items != n_items_ref,
      reason_na = dplyr::case_when(
        missing ~ "missing",
        zero_ref ~ "zero_reference",
        zero_model ~ "zero_model",
        .default = NA_character_
      ),
      rel_mse = dplyr::if_else(is.na(reason_na), exp(log_ratio), NA_real_),
      # gm_mse needs only the model's own errors, so a zero reference does not make it undefined
      # we use that for sensitivity analyses in the meta-regression
      gm_mse = dplyr::if_else(!missing & !zero_model, exp(log_mse), NA_real_)
    ) |>
    dplyr::select(dataset_id, id, model, set, rel_mse, gm_mse, reason_na)
}

# share of lasso_var predictions that equal the mean model's, per dataset x set. an upper bound on
# the share of empty selections: with every slope shrunk to zero the intercept is the training mean,
# and a nonempty fit only matches when beta' (x_t - training mean of x) happens to be 0.
# oos needs dataset_id, id, variable, set, t, model, yhat
lasso_mean_share <- function(oos, tol = 1e-10) {
  key <- c("dataset_id", "id", "variable", "set", "t")
  lasso <- oos |>
    dplyr::filter(model == "lasso_var") |>
    dplyr::select(dplyr::all_of(key), yhat_lasso = yhat)
  reference <- oos |>
    dplyr::filter(model == "mean") |>
    dplyr::select(dplyr::all_of(key), yhat_mean = yhat)
  # a lasso_var row without a mean row is a bug upstream; mean rows without lasso_var rows are
  # datasets where lasso_var failed, recorded in model_failures
  dplyr::inner_join(lasso, reference, by = key, relationship = "one-to-one",
                    unmatched = c("error", "drop")) |>
    dplyr::summarise(n_predictions = dplyr::n(),
                     share_equals_mean = mean(abs(yhat_lasso - yhat_mean) < tol),
                     .by = c(dataset_id, set))
}

# adds <var>_between (group mean, centred with every group weighted equally) and
# <var>_within (deviation from the group mean)
split_between_within <- function(data, var, group) {
  group_means <- data |>
    dplyr::summarise(group_mean = mean({{ var }}, na.rm = TRUE), .by = {{ group }})
  centre <- mean(group_means$group_mean, na.rm = TRUE)

  # add the between and within variables to the original data
  # names depending on input
  data |>
    dplyr::mutate(
      "{{ var }}_between" := mean({{ var }}, na.rm = TRUE) - centre,
      "{{ var }}_within" := {{ var }} - mean({{ var }}, na.rm = TRUE),
      .by = {{ group }}
    )
}
