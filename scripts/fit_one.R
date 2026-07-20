# fit_one.R
# fit all active models for one dataset
#
# Usage:  Rscript scripts/fit_one.R <dataset_id>
# Called by the Makefile pattern rule for output/results/%.rds.
# Parallelise across datasets with `make -jN`
library(here)

source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "metrics.R"))
source(here::here("scripts", "engine", "run_dataset.R"))

args <- commandArgs(trailingOnly = TRUE)
dataset_id <- if (length(args) >= 1) args[[1]] else "mock01"

set.seed(cfg$seed)
interim <- readRDS(file.path("data", "interim", paste0(dataset_id, ".rds")))
result <- run_dataset(interim, cfg)
saveRDS(result, file.path("output", "results", paste0(dataset_id, ".rds")))
message(sprintf("fit %s: %d persons x %d models",
                dataset_id, result$meta$n_person, length(cfg$active_models)))
