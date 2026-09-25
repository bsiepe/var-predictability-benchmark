# 03_collect_results.R
# assembles analysis-ready tables from output/results/*.rds
#
# outputs:
#   output/analysis/metrics.rds: person x model x variable x set (primary table)
#   output/analysis/metrics.csv: same, for inspection / cross-tool use
#   output/analysis/dataset_meta.rds: dataset-level summary with nested model_failures
#   output/meta/combined.rds: person x model x set with R2, RMSE, n, and moderators

library(here)
library(dplyr)
library(purrr)
library(readr)

MODEL_LEVELS <- c("mean", "locf", "trend", "ri", "ar", "var", "ml_ar", "ml_var")

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
# globally-constant persons removed in preprocessing; fold-level ss_tot == 0
# produces R² = NA via compute_metrics() guard
metrics <- map(results, \(r) mutate(r$metrics_var, dataset_id = r$dataset_id,
                                    id = as.character(id),
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
                     show_col_types = FALSE) |>
  mutate(dataset_id = sprintf("%04d", as.integer(dataset_id)))

dataset_meta <- map(results, function(r) {
  tibble(
    dataset_id = r$dataset_id,
    n_persons_kept = r$meta$n_person,
    n_persons_excluded  = nrow(r$meta$excluded),
    n_excluded_low_obs = sum(r$meta$excluded$reason == "low_obs"),
    n_excluded_zero_var = sum(r$meta$excluded$reason == "zero_var"),
    n_items = length(unique(r$metrics_var$variable)),
    ml_var_uncorrelated = r$meta$ml_var_uncorrelated %||% NA,
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

# --- person-level meta (series lengths) ---
person_meta <- purrr::map(results, function(r) {
  nv <- r$meta$n_valid
  nt <- r$meta$n_total
  ni <- r$meta$n_imputed
  dplyr::tibble(
    dataset_id = r$dataset_id,
    id = names(nv),
    n_valid_person = as.integer(nv),
    n_total_person = as.integer(nt),
    n_imputed = if (!is.null(ni)) as.integer(ni[names(nv)]) else 0L
  )
}) |> dplyr::bind_rows()

# --- fitting issues (lme4 singular fits, convergence warnings) ---
# n_fits is the denominator per model level: person-level fit calls for person-level models,
# lmer calls (one per item and fit step) for dataset-level models
fit_issues <- map(results, function(r) {
  if (is.null(r$meta$fit_issues)) return(NULL)
  mutate(r$meta$fit_issues, dataset_id = r$dataset_id,
         n_fits = if_else(level == "person", r$meta$n_person_fits,
                          r$meta$n_fit_steps * r$meta$p),
         .before = 1)
}) |>
  bind_rows()
missing_issues <- map_chr(keep(results, \(r) is.null(r$meta$fit_issues)), "dataset_id")
if (length(missing_issues) > 0)
  warning("missing fit_issues (re-run fit_one.R): ", paste(missing_issues, collapse = ", "))

# --- save ---
dir.create(here("output", "analysis"), showWarnings = FALSE, recursive = TRUE)
saveRDS(fit_issues, here("output", "analysis", "fit_issues.rds"))
saveRDS(metrics, here("output", "analysis", "metrics.rds"))
saveRDS(dataset_meta, here("output", "analysis", "dataset_meta.rds"))
# factor written as character in CSV; load RDS to preserve level order
write_csv(mutate(metrics, model = as.character(model)),
          here("output", "analysis", "metrics.csv"))

message(sprintf("collected %d datasets, %d rows in metrics",
                nrow(dataset_meta), nrow(metrics)))

# --- person-level aggregation for meta-analysis ---
person_metrics <- metrics |>
  group_by(dataset_id, id, model, set) |>
  summarise(
    R2   = 1 - sum(ss_res) / sum(ss_tot),
    rmse = if (sum(n) > 0) sqrt(sum(ss_res) / sum(n)) else NA_real_,
    n    = sum(n),
    .groups = "drop"
  ) |>
  left_join(
    select(dataset_meta, dataset_id, first_author, year,
           n_beeps_per_day, n_time_points, n_participants,
           sampling_scheme, participants, lag_mode, p = n_items),
    by = "dataset_id"
  ) |>
  left_join(person_meta, by = c("dataset_id", "id")) |>
  mutate(n_oos_tp = if_else(set == "oos", n / p, NA_real_))

dir.create(here("output", "meta"), showWarnings = FALSE, recursive = TRUE)
non_int <- person_metrics[!is.na(person_metrics$n_oos_tp) &
                            person_metrics$n_oos_tp != floor(person_metrics$n_oos_tp), ]
if (nrow(non_int) > 0)
  warning(nrow(non_int), " OOS rows with non-integer n_oos_tp (variable-unequal missingness): ",
          paste(unique(non_int$dataset_id), collapse = ", "))

saveRDS(person_metrics, here("output", "meta", "combined.rds"))
message(sprintf("combined.rds: %d rows, %d datasets, %d models",
                nrow(person_metrics),
                n_distinct(person_metrics$dataset_id),
                n_distinct(person_metrics$model)))
