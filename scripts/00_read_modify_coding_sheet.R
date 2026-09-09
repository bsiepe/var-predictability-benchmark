# per-dataset modifiers for include = "modify" datasets.
# each modifier: function(df, features) -> list(df = ..., features = ...)
# use dplyr::bind_rows() when adding feature rows (fills missing columns with NA)
# to activate: code modifier, verify variable names, flip TSV include to "yes"
# keys are zero-padded 4-digit dataset IDs

library(dplyr)
library(readr)
library(here)

coding_sheet_path <- here::here("data", "meta", "datasets.tsv")

dataset_modifiers <- list()