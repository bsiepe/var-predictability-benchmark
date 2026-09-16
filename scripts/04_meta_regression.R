# 04_meta_regression.R
# Aggregates output/analysis/metrics.rds to person-level outcomes and joins
# study-level moderators. Saves output/meta/combined.rds for downstream analysis.
#
# outputs:
#   output/meta/combined.rds: person x model x set with R2, RMSE, n, and moderators


# --- Prep ----
library(here)
library(dplyr)

metrics <- readRDS(here("output", "analysis", "metrics.rds"))
dataset_meta <- readRDS(here("output", "analysis", "dataset_meta.rds"))


# --- Checks and aggregation ---
required_cols <- c("dataset_id", "id", "model", "set", "ss_res", "ss_tot", "n")
missing_cols <- setdiff(required_cols, names(metrics))
if (length(missing_cols) > 0)
  stop("metrics.rds missing columns: ", paste(missing_cols, collapse = ", "))

# aggregate to person-level; carry model factor levels from metrics
person_metrics <- metrics |>
  group_by(dataset_id, id, model, set) |>
  summarise(
    R2   = 1 - sum(ss_res) / sum(ss_tot),
    RMSE = if (sum(n) > 0) sqrt(sum(ss_res) / sum(n)) else NA_real_,
    n    = sum(n),   # observation count (across variables)
    .groups = "drop"
  )

# join study-level moderators; n_items from dataset_meta comes from interim$items
# (after modifiers), so it correctly reflects the actual p used in models
n_before <- nrow(person_metrics)

person_metrics <- person_metrics |>
  left_join(
    select(dataset_meta, dataset_id, n_beeps_per_day, n_time_points,
           n_participants, lag_mode, p = n_items),
    by = "dataset_id"
  )

if (nrow(person_metrics) != n_before)
  stop("join changed row count: ", n_before, " -> ", nrow(person_metrics))

# Check for missing moderators
missing_mods <- person_metrics |>
  filter(is.na(n_beeps_per_day) | is.na(n_time_points) | is.na(p)) |>
  distinct(dataset_id) |>
  pull(dataset_id)
if (length(missing_mods) > 0)
  warning("missing moderators for: ", paste(missing_mods, collapse = ", "))

# save file and generate summary
saveRDS(person_metrics, here("output", "meta", "combined.rds"))
message(sprintf("combined.rds: %d rows, %d datasets, %d models",
                nrow(person_metrics),
                n_distinct(person_metrics$dataset_id),
                n_distinct(person_metrics$model)))
