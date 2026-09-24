# ---- Features Computation ----
# builds descriptive series features and time-indexed OOS predictions for the report
# we need these for later plots and tables, but they are not used in model fitting
#
# Usage:  Rscript scripts/03b_features.R <dataset_id> [<dataset_id> ...]
# Called by the Makefile target output/analysis/features.rds, after fitting.
# Reads data/interim/<id>.rds (the exact series the models saw) and output/results/<id>.rds.
# Stops on any missing input or failed check; never writes a partial cache.
#
# output: output/analysis/features.rds, a list of tables (grains in feature_keys)
#   item        dataset_id, id, variable
#   person      dataset_id, id
#   item_error  dataset_id, id, variable, model   OOS forecast-error decomposition
#   series      dataset_id, id, variable, t       observed series (for plotting)
#   oos         dataset_id, id, variable, model, t
#   provenance  settings and input file hashes the cache was built from

library(here)

source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "features.R"))

ids <- commandArgs(trailingOnly = TRUE)
if (length(ids) == 0) stop("usage: Rscript scripts/03b_features.R <dataset_id> ...")
if (anyDuplicated(ids) > 0) stop("duplicated dataset ids: ", paste(ids[duplicated(ids)], collapse = ", "))

# load all datasets and prediction results
interim_files <- here::here("data", "interim", paste0(ids, ".rds"))
result_files <- here::here("output", "results", paste0(ids, ".rds"))
missing_files <- c(interim_files[!file.exists(interim_files)],
                   result_files[!file.exists(result_files)])
if (length(missing_files) > 0)
  stop("missing input files:\n", paste(missing_files, collapse = "\n"))

# validate that the result and interim files are aligned, and build feature tables
per_dataset <- lapply(seq_along(ids), function(k) {
  out <- dataset_features(ids[k], readRDS(interim_files[k]), readRDS(result_files[k]), cfg$cv)
  message(sprintf("%s: %d persons, %d items, %d OOS rows", ids[k], nrow(out$person),
                  nrow(out$item), nrow(out$oos)))
  out
})

# combine all datasets into one feature object
bind_all <- function(name) dplyr::bind_rows(lapply(per_dataset, `[[`, name))
features <- list(
  item = bind_all("item"),
  person = bind_all("person"),
  item_error = bind_all("item_error"),
  series = bind_all("series"),
  oos = bind_all("oos"),
  # create a provenance table that tracks the config and input files used to build this object
  provenance = list(
    cv = cfg$cv,
    preprocess = stats::setNames(lapply(per_dataset, `[[`, "settings"), ids),
    dataset_ids = ids,
    interim_md5 = stats::setNames(unname(tools::md5sum(interim_files)), ids),
    result_md5 = stats::setNames(unname(tools::md5sum(result_files)), ids),
    created = Sys.time()
  )
)

dir.create(here::here("output", "analysis"), showWarnings = FALSE, recursive = TRUE)
saveRDS(features, here::here("output", "analysis", "features.rds"))
message(sprintf("features.rds: %d datasets, %d persons, %d items, %d OOS rows",
                length(ids), nrow(features$person), nrow(features$item),
                nrow(features$oos)))
