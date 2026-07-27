# Makefile for benchmarking pipeline
#
# Orchestrates the per-dataset pipeline with file-level dependency tracking
# Expensive fitting is cached per dataset (output/results/<id>.rds); reports read caches
# Parallelise across datasets with:  make -j4
#
# NOTE: this is still a SKELETON. Targets and variables below sketch the intended graph;
# needs to be filled once the rest of the pipeline is implemented.

# ---- Configuration -------------------------------------------------------------
RSCRIPT      := Rscript

# Engine files: editing any of these should invalidate downstream results.
ENGINE_DEPS  := scripts/engine/config.R scripts/engine/preprocess.R \
                scripts/engine/models.R scripts/engine/crossval.R \
                scripts/engine/metrics.R scripts/engine/run_dataset.R
PREP_DEPS    := scripts/engine/config.R scripts/engine/preprocess.R

# dataset IDs are read from data/meta/datasets.csv.
DATASET_IDS  := $(shell $(RSCRIPT) -e "x <- read.csv('data/meta/datasets.csv'); cat(x[['dataset_id']], sep=' ')")

INTERIM      := $(patsubst %,data/interim/%.rds,$(DATASET_IDS))
RESULTS      := $(patsubst %,output/results/%.rds,$(DATASET_IDS))

# ---- Phony targets -------------------------------------------------------------
.PHONY: all preprocess fit meta reports clean restore
all: reports

preprocess: $(INTERIM)
fit: $(RESULTS)

# ---- Pattern rules -------------------------------------------------------------
data/interim/%.rds: data/meta/datasets.csv $(PREP_DEPS)
	$(RSCRIPT) scripts/preprocess_one.R $*

output/results/%.rds: data/interim/%.rds $(ENGINE_DEPS)
	$(RSCRIPT) scripts/fit_one.R $*

# ---- Aggregation & reports -----------------------------------------------------
output/meta/combined.rds: $(RESULTS) scripts/04_meta_regression.R
	$(RSCRIPT) scripts/04_meta_regression.R

meta: output/meta/combined.rds

reports: meta
	quarto render scripts/05_results.qmd
	quarto render scripts/02_descriptives.qmd

# ---- Housekeeping --------------------------------------------------------------
restore:
	$(RSCRIPT) -e 'renv::restore()'

clean:
	rm -f data/interim/*.rds output/results/*.rds output/meta/*.rds
