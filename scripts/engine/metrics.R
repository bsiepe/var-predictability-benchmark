#--------- Performance metrics ---------#
# Contains functions to compute performance metrics for the model predictions.

# Compute sum of squares by person and variable
.ss_by_pv <- function(tab) {
  tab |>
    dplyr::filter(is.finite(yhat), is.finite(y)) |>
    dplyr::summarise(
      ss_res = sum((y - yhat)^2),
      ss_tot = sum((y - mean(y))^2),
      n = dplyr::n(),
      .by = c(person, variable, set)
    )
}

# Compute R2 and standardized RMSE by person and variable
compute_metrics <- function(oos_table) {
  pv <- .ss_by_pv(oos_table)
  person <- pv |>
    dplyr::summarise(ss_res = sum(ss_res), ss_tot = sum(ss_tot), n = sum(n),
                     .by = c(person, set)) |>
    dplyr::mutate(
      R2 = 1 - ss_res / ss_tot,
      stdRMSE = sqrt(ss_res / n)
    ) |>
    dplyr::select(person, set, R2, stdRMSE, n)
  list(person = person, person_variable = pv)
}
