library(here)
source(here::here("scripts", "engine", "relative.R"))

expect_error <- function(expr, pattern) {
  msg <- tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  stopifnot(!is.null(msg), grepl(pattern, msg))
}

# one person, two items, item MSE given directly (n = 10, ss_res = 10 * mse)
item_rows <- function(id, model, mse, n = c(10, 10), variable = c("x1", "x2")) {
  dplyr::tibble(dataset_id = "0001", id = id, model = model, variable = variable,
                set = "oos", ss_res = mse * n, n = n)
}
get <- function(out, id, model, col) out[[col]][out$id == id & out$model == model]

metrics <- dplyr::bind_rows(
  item_rows("p1", "mean", c(0.04, 0.01)),
  item_rows("p1", "ar", c(0.01, 0.01)),     # ratios 0.25 and 1
  item_rows("p1", "var", c(0.02, 0.005)),   # ratios 0.5 and 0.5
  item_rows("p2", "mean", c(0.04, 0.01)),
  item_rows("p2", "locf", c(0, 0.02)),      # perfect on x1
  item_rows("p2", "ar", c(0.02, 0.01)),
  item_rows("p3", "mean", c(0, 0.01)),      # x1 constant: reference MSE 0
  item_rows("p3", "ar", c(0, 0.02)),
  item_rows("p4", "mean", c(0.04, 0.01)),
  item_rows("p4", "ar", 0.02, n = 10, variable = "x1"),  # no predictions for x2
  item_rows("p5", "mean", c(0.04, 0.01)),
  item_rows("p5", "ar", c(0.02, 0.01), n = c(0, 10))     # x2 scored, x1 has no test rows
)
metrics$ss_res[metrics$id == "p5" & metrics$model == "ar" & metrics$variable == "x1"] <- 0
out <- relative_mse(metrics)

# 1. known ratios: geometric mean of 0.25 and 1 is 0.5
stopifnot(isTRUE(all.equal(get(out, "p1", "ar", "rel_mse"), 0.5)),
          isTRUE(all.equal(get(out, "p1", "var", "rel_mse"), 0.5)))
cat("geometric mean of item ratios: PASS\n")

# 2. the mean model has rel_mse 1 wherever it is defined
stopifnot(all(out$rel_mse[out$model == "mean"] == 1, na.rm = TRUE))
cat("mean model rel_mse = 1: PASS\n")

# 3. undefined cases get NA with the right reason, only for the affected person x model
stopifnot(get(out, "p2", "locf", "reason_na") == "zero_model",
          is.na(get(out, "p2", "locf", "rel_mse")),
          is.na(get(out, "p2", "ar", "reason_na")),
          isTRUE(all.equal(get(out, "p2", "ar", "rel_mse"), sqrt(0.5 * 1))),
          get(out, "p3", "ar", "reason_na") == "zero_reference",
          get(out, "p4", "ar", "reason_na") == "missing",
          get(out, "p5", "ar", "reason_na") == "missing",
          all(is.finite(out$rel_mse) | is.na(out$rel_mse)),
          all(is.finite(out$gm_mse) | is.na(out$gm_mse)))
cat("zero and missing cases flagged per person x model: PASS\n")

# 4. step ratio: rel_mse(var) / rel_mse(ar) is the geometric mean of MSE_var / MSE_ar
step <- get(out, "p1", "var", "rel_mse") / get(out, "p1", "ar", "rel_mse")
stopifnot(isTRUE(all.equal(step, exp(mean(log(c(0.02, 0.005) / c(0.01, 0.01)))))))
cat("step ratio identity: PASS\n")

# 5. one row per person x model x set
stopifnot(anyDuplicated(out[c("dataset_id", "id", "model", "set")]) == 0,
          nrow(out) == nrow(dplyr::distinct(metrics, dataset_id, id, model, set)))
cat("unique keys: PASS\n")

# 6. a model scored on a different number of test rows than the mean model stops
metrics_bad <- dplyr::bind_rows(item_rows("p1", "mean", c(0.04, 0.01)),
                                item_rows("p1", "ar", c(0.01, 0.01), n = c(9, 10)))
expect_error(relative_mse(metrics_bad), "different number of test rows")
cat("row-count mismatch rejected: PASS\n")

# 7. gm_mse(model) / gm_mse(mean) reproduces rel_mse
defined <- out[!is.na(out$rel_mse) & out$model != "mean", ]
gm_ref <- out[out$model == "mean", c("id", "gm_mse")]
cmp <- merge(defined, gm_ref, by = "id", suffixes = c("", "_ref"))
stopifnot(nrow(cmp) > 0, isTRUE(all.equal(cmp$gm_mse / cmp$gm_mse_ref, cmp$rel_mse)))
cat("gm_mse ratio equals rel_mse: PASS\n")

# 8. between/within split: two groups of unequal size, so equal-weight centring
#    (group means 2 and 15, centre 8.5) differs from the grand mean (7.2)
persons <- dplyr::tibble(group = c("a", "a", "a", "b", "b"), x = c(1, 2, 3, 10, 20))
split <- split_between_within(persons, x, group)
stopifnot(isTRUE(all.equal(split$x_between, c(-6.5, -6.5, -6.5, 6.5, 6.5))),
          isTRUE(all.equal(split$x_within, c(-1, 0, 1, -5, 5))),
          isTRUE(all.equal(split$x_between + split$x_within + 8.5, persons$x)))
cat("between/within split with equal group weights: PASS\n")

# 9. a missing value stays missing and does not shift its group's mean
split_na <- split_between_within(dplyr::mutate(persons, x = c(1, NA, 3, 10, 20)), x, group)
stopifnot(is.na(split_na$x_within[2]), !is.na(split_na$x_between[2]),
          isTRUE(all.equal(split_na$x_within[c(1, 3)], c(-1, 1))))
cat("between/within split with missing values: PASS\n")

cat("all relative MSE tests passed\n")
