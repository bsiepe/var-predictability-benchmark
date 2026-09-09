# 03_collect_results.R
# assembles analysis-ready tables from output/results/*.rds
#
# outputs:
#   output/analysis/metrics.rds: person x model x variable x set (primary table)
#   output/analysis/metrics.csv: same, for inspection / cross-tool use
#   output/analysis/dataset_meta.rds: dataset-level summary with nested model_failures
#
# note: metrics contains ss_res, ss_tot, n and R2_by_variable (per-variable R²).
# to obtain person-level R², re-aggregate as 1 - sum(ss_res) / sum(ss_tot) over variables.
# do NOT average R2_by_variable — denominators differ across variables.

library(here)
library(dplyr)
library(purrr)
library(readr)

MODEL_LEVELS <- c("mean", "trend", "ri", "ar", "var", "ml_ar", "ml_var")

result_files <- list.files(here("output", "results"), pattern = "\\.rds$",
                           full.names = TRUE)
if (length(result_files) == 0) stop("no result files found in output/results/")

# load results; skip corrupt files rather than aborting
results <- map(result_files, function(f) {
  tryCatch(readRDS(f), error = function(e) {
    warning("skipping ", basename(f), ": ", e$message); NULL
  })
}) |> compact()
message("loaded ", length(results), " / ", length(result_files), " result files")

# warn about results built with old engine (no metrics_var field)
old_ids <- map_chr(keep(results, \(r) is.null(r$metrics_var)), "dataset_id")
if (length(old_ids) > 0)
  warning("missing metrics_var (re-run fit_one.R): ", paste(old_ids, collapse = ", "))
results <- discard(results, \(r) is.null(r$metrics_var))
if (length(results) == 0) stop("no usable result files after filtering")

# --- primary metrics table ---
# Keep all rows, including degenerate cases where ss_tot == 0.
# Those rows will naturally produce R² = NaN/Inf and can be filtered later if a
# particular analysis requires finite values
metrics <- map(results, \(r) mutate(r$metrics_var, dataset_id = r$dataset_id,
                                    .before = 1)) |>
  bind_rows() |>
  mutate(
    R2_by_variable = 1 - ss_res / ss_tot,
    model = factor(model, levels = intersect(MODEL_LEVELS, unique(model)))
  ) |>
  select(dataset_id, id, model, variable, set, ss_res, ss_tot, n, R2_by_variable)

if (nrow(metrics) == 0) stop("metrics table is empty")

# --- dataset-level summary ---
registry <- read_tsv(here("data", "meta", "datasets.tsv"),
                     col_types = cols(dataset_id = col_character()),
                     show_col_types = FALSE)

dataset_meta <- map(results, function(r) {
  tibble(
    dataset_id = r$dataset_id,
    n_persons_kept = r$meta$n_person,
    n_persons_excluded  = length(r$meta$excluded),
    n_items = length(unique(r$metrics_var$variable)),
    model_failures = list(r$meta$model_failures)  # named logical vector; unnest to analyse
  )
}) |>
  bind_rows() |>
  left_join(
    select(registry, dataset_id, first_author, year, lag_mode,
           n_participants, n_time_points, n_beeps_per_day,
           sampling_scheme, participants, model_original),
    by = "dataset_id"
  )

unmatched <- dataset_meta$dataset_id[is.na(dataset_meta$first_author)]
if (length(unmatched) > 0)
  warning("dataset_ids not found in datasets.tsv: ", paste(unmatched, collapse = ", "))

# --- save ---
dir.create(here("output", "analysis"), showWarnings = FALSE, recursive = TRUE)
saveRDS(metrics, here("output", "analysis", "metrics.rds"))
saveRDS(dataset_meta, here("output", "analysis", "dataset_meta.rds"))
# factor written as character in CSV; load RDS to preserve level order
write_csv(mutate(metrics, model = as.character(model)),
          here("output", "analysis", "metrics.csv"))

message(sprintf("collected %d datasets, %d rows in metrics",
                nrow(dataset_meta), nrow(metrics)))
