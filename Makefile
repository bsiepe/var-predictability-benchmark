# Makefile for benchmarking pipeline
#
# Orchestrates the per-dataset pipeline with file-level dependency tracking
# Expensive fitting is cached per dataset (output/results/<id>.rds); reports read caches
# Parallelise across datasets with:  make -j4
#
# Must be run from the project root. The project .Rprofile activates renv.

# ---- Configuration -------------------------------------------------------------
# Override with `make RSCRIPT=/path/to/Rscript` when Rscript is not on PATH.
RSCRIPT      ?= Rscript

# Engine files: editing any of these should invalidate downstream results.
ENGINE_DEPS  := scripts/engine/config.R scripts/engine/preprocess.R \
                scripts/engine/models.R scripts/engine/crossval.R \
                scripts/engine/metrics.R scripts/engine/run_dataset.R
# Preprocessing also depends on the modifier script: changing a modifier must
# trigger a rebuild of interim files
PREP_DEPS    := scripts/engine/config.R scripts/engine/preprocess.R \
                scripts/00_read_modify_coding_sheet.R

# Dataset IDs are read from data/meta/datasets.tsv, filtered to include == "yes"
# or "modify", and zero-padded to 4 digits (openESM requires "0001" not "1").
# "modify" means the dataset is included but requires derived variables defined in
# scripts/00_read_modify_coding_sheet.R. "unclear"/"no" rows are excluded.
DATASET_IDS  := $(shell $(RSCRIPT) --vanilla -e \
  "x <- utils::read.delim('data/meta/datasets.tsv', stringsAsFactors=FALSE, \
   colClasses=c(dataset_id='character')); \
   ids <- x[x[['include']] %in% c('yes', 'modify'), 'dataset_id']; \
   cat(sprintf('%04d', as.integer(ids)), sep=' ')")

INTERIM      := $(patsubst %,data/interim/%.rds,$(DATASET_IDS))
RESULTS      := $(patsubst %,output/results/%.rds,$(DATASET_IDS))
FEATURES     := output/analysis/features.rds

# ---- Phony targets -------------------------------------------------------------
.PHONY: all preprocess fit meta features reports clean rerun restore
all: reports

preprocess: $(INTERIM)
fit: $(RESULTS)

# ---- Pattern rules -------------------------------------------------------------
data/interim/%.rds: data/meta/datasets.tsv $(PREP_DEPS)
	$(RSCRIPT) scripts/preprocess_one.R $*

output/results/%.rds: data/interim/%.rds $(ENGINE_DEPS)
	$(RSCRIPT) scripts/fit_one.R $* > output/logs/$*.log 2>&1

# ---- Aggregation & reports -----------------------------------------------------
output/meta/combined.rds: $(RESULTS) scripts/03_collect_results.R
	$(RSCRIPT) scripts/03_collect_results.R

meta: output/meta/combined.rds

# series features and time-indexed OOS predictions; needs both interim and result caches
$(FEATURES): $(INTERIM) $(RESULTS) scripts/03b_features.R scripts/engine/features.R \
             scripts/engine/config.R scripts/engine/crossval.R
	$(RSCRIPT) scripts/03b_features.R $(DATASET_IDS)

features: $(FEATURES)

reports: meta features
	quarto render scripts/05_results.qmd
	quarto render scripts/02_descriptives.qmd

# ---- Housekeeping --------------------------------------------------------------
restore:
	$(RSCRIPT) -e "renv::restore()"

clean:
	rm -f data/interim/*.rds output/results/*.rds output/meta/*.rds $(FEATURES)

rerun:
	$(MAKE) clean
	$(MAKE) -k all
