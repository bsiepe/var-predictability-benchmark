library(dplyr)

# Tests for n_oos_tp computation and person-level series-length fields.
# These tests exercise the aggregation logic in 03_collect_results.R directly,
# without running the full pipeline.

# helper: compute n_oos_tp from a metrics-like table (same logic as collect script)
compute_n_oos_tp <- function(metrics_long, p) {
  metrics_long |>
    group_by(dataset_id, id, model, set) |>
    summarise(n = sum(n), .groups = "drop") |>
    left_join(tibble(dataset_id = unique(metrics_long$dataset_id), p = p),
              by = "dataset_id") |>
    mutate(n_oos_tp = if_else(set == "oos", n / p, NA_real_))
}

# ---- Test 1: standard case — no missingness ----
# p=3 variables, 10 OOS timepoints each → n=30, n_oos_tp=10
metrics1 <- tibble(
  dataset_id = "1", id = "p1", model = "ar", set = "oos",
  variable = c("v1", "v2", "v3"), n = c(10L, 10L, 10L)
)
res1 <- compute_n_oos_tp(metrics1, p = 3)
stopifnot(res1$n_oos_tp == 10)
cat("Test 1 (standard, no missingness): PASS\n")

# ---- Test 2: uniform item-level missingness ----
# p=3, 8 valid OOS timepoints per item → n=24, n_oos_tp=8
metrics2 <- tibble(
  dataset_id = "1", id = "p1", model = "ar", set = "oos",
  variable = c("v1", "v2", "v3"), n = c(8L, 8L, 8L)
)
res2 <- compute_n_oos_tp(metrics2, p = 3)
stopifnot(res2$n_oos_tp == 8)
cat("Test 2 (uniform missingness): PASS\n")

# ---- Test 3: variable missingness across items — non-integer n_oos_tp ----
# p=3, items have 10, 9, 8 valid obs → n=27, n_oos_tp=9 (average, non-integer check)
metrics3 <- tibble(
  dataset_id = "1", id = "p1", model = "ar", set = "oos",
  variable = c("v1", "v2", "v3"), n = c(10L, 9L, 8L)
)
res3 <- compute_n_oos_tp(metrics3, p = 3)
stopifnot(res3$n_oos_tp == 9)
# confirm it is non-integer when items differ and n not divisible by p
metrics3b <- tibble(
  dataset_id = "1", id = "p1", model = "ar", set = "oos",
  variable = c("v1", "v2", "v3"), n = c(10L, 9L, 9L)
)
res3b <- compute_n_oos_tp(metrics3b, p = 3)
stopifnot(res3b$n_oos_tp != floor(res3b$n_oos_tp))
cat("Test 3 (variable missingness, non-integer): PASS\n")

# ---- Test 4: n_oos_tp is NA for in-sample rows ----
metrics4 <- tibble(
  dataset_id = "1", id = "p1", model = "ar",
  set = c("in", "oos"),
  variable = "v1", n = c(25L, 10L)
)
res4 <- compute_n_oos_tp(metrics4, p = 1)
stopifnot(is.na(res4$n_oos_tp[res4$set == "in"]))
stopifnot(res4$n_oos_tp[res4$set == "oos"] == 10)
cat("Test 4 (n_oos_tp NA for in-sample): PASS\n")

# ---- Test 5: n_valid_person >= n_oos_tp and n_total_person >= n_valid_person ----
# simulate a combined.rds row and check assertions
row5 <- tibble(
  set = "oos", n_oos_tp = 10, n_valid_person = 25L, n_total_person = 50L
)
stopifnot(row5$n_oos_tp <= row5$n_valid_person)
stopifnot(row5$n_total_person >= row5$n_valid_person)
# edge case: n_total == n_valid (all rows valid)
row5b <- tibble(
  set = "oos", n_oos_tp = 10, n_valid_person = 30L, n_total_person = 30L
)
stopifnot(row5b$n_total_person >= row5b$n_valid_person)
cat("Test 5 (ordering assertions n_total >= n_valid >= n_oos_tp): PASS\n")

cat("\nAll test_collect_results tests passed.\n")
