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

if (startsWith(dataset_id, "mock")) {
  mock <- make_mock_openesm(seed = 1)
  df <- mock$data
  features <- mock$meta
} else {
  dataset <- openesm::get_dataset(dataset_id)
  df <- dataset$data
  features <- dataset$metadata$features[[1]]
}

interim <- preprocess_dataset(df, features, cfg, dataset_id = dataset_id)
message(sprintf("preprocessed %s: %d persons kept, %d excluded",
                dataset_id, length(interim$persons), length(interim$excluded)))
saveRDS(interim, file.path("data", "interim", paste0(dataset_id, ".rds")))
