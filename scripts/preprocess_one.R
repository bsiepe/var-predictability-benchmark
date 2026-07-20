# preprocess_one.R
# CLI runner: build data/interim/<id>.rds for one dataset.
#
# Usage: Rscript scripts/preprocess_one.R <dataset_id>
# Called by the Makefile pattern rule for data/interim/%.rds.
#
# IDs starting with "mock" are generated in-memory. Real IDs load a tsv + item
# metadata from data/raw/<id>/; swap in openesm::get_dataset() when getting live data

library(here)

source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "harmonize.R"))
source(here::here("scripts", "engine", "preprocess.R"))

args <- commandArgs(trailingOnly = TRUE)
dataset_id <- if (length(args) >= 1) args[[1]] else "mock01"

if (startsWith(dataset_id, "mock")) {
  mock <- make_mock_openesm(seed = 1)
  df <- mock$data
  item_meta <- mock$meta
  source_tag <- "mock"
} else {
  df <- utils::read.delim(file.path("data", "raw", dataset_id, "data.tsv"))
  item_meta <- utils::read.delim(file.path("data", "raw", dataset_id, "items.tsv"))
  source_tag <- "openesm"
}

interim <- preprocess_dataset(df, item_meta, cfg, dataset_id = dataset_id, source = source_tag)
message(sprintf("preprocessed %s: %d persons kept, %d excluded",
                dataset_id, length(interim$persons), length(interim$excluded)))
saveRDS(interim, file.path("data", "interim", paste0(dataset_id, ".rds")))
