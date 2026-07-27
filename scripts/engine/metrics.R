#--------- Performance metrics ---------#
# Contains functions to compute performance metrics for the model predictions.

# Compute sum of squares by person and variable.
# ss_tot is anchored on the in-sample mean for both sets, so in-sample and OOS
# R² are on the same scale: R² = 0 ↔ mean model, R² > 0 ↔ beats mean model.
.ss_by_pv <- function(tab) {
  mu_train <- tab |>
    dplyr::filter(set == "in", is.finite(y)) |>
    dplyr::summarise(mu_train = mean(y), .by = c(id, variable))
  tab |>
    dplyr::filter(is.finite(yhat), is.finite(y)) |>
    dplyr::left_join(mu_train, by = c("id", "variable")) |>
    dplyr::summarise(
      ss_res = sum((y - yhat)^2),
      ss_tot = sum((y - mu_train)^2),
      n = dplyr::n(),
      .by = c(id, variable, set)
    )
}

# Compute R2 and standardized RMSE by person and variable
compute_metrics <- function(oos_table) {
  pv <- .ss_by_pv(oos_table)
  by_id <- pv |>
    dplyr::summarise(ss_res = sum(ss_res), ss_tot = sum(ss_tot), n = sum(n),
                     .by = c(id, set)) |>
    dplyr::mutate(
      R2 = 1 - ss_res / ss_tot,
      stdRMSE = sqrt(ss_res / n)
    ) |>
    dplyr::select(id, set, R2, stdRMSE, n)
  list(by_id = by_id, by_id_variable = pv)
}
