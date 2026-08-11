# scripts/00_init_coding_sheet.R
#
# Run once from the project root to scaffold data/meta/datasets.xlsx.
# Pulls openESM metadata and adds empty manual coding columns.
# Manual columns are filled by hand — do NOT re-run on a filled sheet.

library(openesm)
library(dplyr)
library(readr)
library(here)

out_path <- here::here("data", "meta", "datasets.tsv")

if (file.exists(out_path)) {
  stop(out_path, " already exists. Delete it manually if you want to regenerate.")
}

list_datasets() |>
  select(
    dataset_id, first_author, year, paper_doi,
    n_participants, n_time_points, n_days, n_beeps_per_day,
    sampling_scheme, participants, topics, link_to_codebook
  ) |>
  mutate(
    model_original     = NA_character_,  # e.g. mlVAR, VAR, DSEM, lagged_regression, none
    variables_original = NA_character_,  # comma-separated items used in that model
    include            = NA_character_,  # yes / no / maybe
    notes              = NA_character_
  ) |>
  write_tsv(out_path)

message("Written to ", out_path)
