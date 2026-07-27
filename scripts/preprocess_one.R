# preprocess_one.R
# CLI runner: build data/interim/<id>.rds for one dataset.
#
# Usage: Rscript scripts/preprocess_one.R <dataset_id>
# Called by the Makefile pattern rule for data/interim/%.rds.

library(here)

source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))

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

  # filter to selected items from registry (empty items column = use all)
  registry <- utils::read.csv(here::here("data", "meta", "datasets.csv"),
                              stringsAsFactors = FALSE,
                              colClasses = c(dataset_id = "character"))
  row <- registry[registry$dataset_id == dataset_id, ]
  if (nrow(row) == 0) stop("dataset_id '", dataset_id, "' not found in data/meta/datasets.csv")
  if (nzchar(trimws(row$items))) {
    selected <- trimws(strsplit(row$items, ",")[[1]])
    features <- features[features$name %in% selected, ]
  }
}

interim <- preprocess_dataset(df, features, cfg, dataset_id = dataset_id)
message(sprintf("preprocessed %s: %d persons kept, %d excluded",
                dataset_id, length(interim$persons), length(interim$excluded)))
saveRDS(interim, file.path("data", "interim", paste0(dataset_id, ".rds")))
