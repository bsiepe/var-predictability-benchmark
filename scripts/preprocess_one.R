# preprocess_one.R
# CLI runner: build data/interim/<id>.rds for one dataset.
#
# Usage: Rscript scripts/preprocess_one.R <dataset_id>
# Called by the Makefile pattern rule for data/interim/%.rds.

library(here)
library(readr)

source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "00_read_modify_coding_sheet.R"))

args <- commandArgs(trailingOnly = TRUE)
dataset_id <- if (length(args) >= 1) args[[1]] else "mock01"
overwrite <- "--overwrite" %in% args

if (startsWith(dataset_id, "mock")) {
  mock <- make_mock_openesm(seed = 1)
  df <- mock$data
  features <- mock$meta
} else {
  # download-and-cache: fetch from openESM on first run, reuse local copy thereafter
  # pass --overwrite to force re-download
  raw_path <- here::here("data", "raw", paste0(dataset_id, ".rds"))
  if (file.exists(raw_path) && !overwrite) {
    dataset <- readRDS(raw_path)
  } else {
    dataset <- openesm::get_dataset(dataset_id)
    saveRDS(dataset, raw_path)
  }
  df <- dataset$data
  features <- dataset$metadata$features[[1]]

  # apply dataset-specific modifier if one exists (adds derived/aggregated columns)
  if (!exists("dataset_modifiers"))
    stop("00_read_modify_coding_sheet.R must define 'dataset_modifiers'")
  if (dataset_id %in% names(dataset_modifiers)) {
    result <- dataset_modifiers[[dataset_id]](df, features)
    if (!is.list(result) || !all(c("df", "features") %in% names(result)))
      stop("modifier for ", dataset_id, " must return list(df = ..., features = ...)")
    df <- result$df
    features <- result$features
  }

  # filter to selected items from registry; match by integer to be padding-format agnostic
  registry <- readr::read_tsv(here::here("data", "meta", "datasets.tsv"),
                              col_types = readr::cols(dataset_id = readr::col_character()))
  row <- registry[as.integer(registry$dataset_id) == as.integer(dataset_id), ]
  if (nrow(row) != 1)
    stop("expected exactly 1 row for dataset_id '", dataset_id, "' in datasets.tsv, got ", nrow(row))

  lag_mode <- if ("lag_mode" %in% names(row) && !is.na(row$lag_mode) && nzchar(trimws(row$lag_mode)))
                trimws(row$lag_mode) else "within_day"
  if (lag_mode == "daily") {
    cfg$preprocess$lag_across_night <- TRUE  # rows span different days by definition
    cfg$preprocess$use_consec_day <- TRUE    # gap check: diff(day)==1, not consec beep
  } else if (lag_mode != "within_day") {
    stop("unknown lag_mode '", lag_mode, "' for dataset ", dataset_id,
         ". valid values: within_day, daily")
  }

  if (!is.na(row$variables_original) && nzchar(trimws(row$variables_original))) {
    selected <- trimws(strsplit(row$variables_original, ",")[[1]])
    missing_names <- setdiff(selected, features$name)
    if (length(missing_names) > 0)
      stop("variables_original references names absent from features for ", dataset_id,
           ": ", paste(missing_names, collapse = ", "))
    features <- features[features$name %in% selected, ]
    if (nrow(features) == 0) stop("no features remain after filtering for ", dataset_id)
    if (anyDuplicated(features$name))
      stop("duplicate feature names after filtering for ", dataset_id)
  }
}

interim <- preprocess_dataset(df, features, cfg, dataset_id = dataset_id)
message(sprintf("preprocessed %s: %d persons kept, %d excluded",
                dataset_id, length(interim$persons), length(interim$excluded)))
saveRDS(interim, file.path("data", "interim", paste0(dataset_id, ".rds")))
