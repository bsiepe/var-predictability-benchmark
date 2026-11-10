# export_references.R
# writes the openESM references (reference_a and reference_b) of all included datasets to one .bib
# file, and the entry keys per dataset for the dataset table
#
# Usage: Rscript scripts/export_references.R

library(here)
library(dplyr)
library(purrr)

read_references <- function(id) {
  dataset <- readRDS(here("data", "raw", paste0(id, ".rds")))
  # cite() also prints the entries through cli; only the returned string is needed
  suppressMessages(openesm::cite(dataset)) %||% NA_character_
}

entry_keys <- function(bib) {
  if (is.na(bib)) return(NA_character_)
  heads <- regmatches(bib, gregexpr("@[[:alpha:]]+\\s*\\{\\s*[^,[:space:]]+", bib))[[1]]
  paste(sub(".*\\{\\s*", "", heads), collapse = ",")
}

# same filter and id padding as the Makefile
registry <- readr::read_tsv(here("data", "meta", "datasets.tsv"),
                            col_types = readr::cols(dataset_id = readr::col_character()),
                            show_col_types = FALSE) |>
  filter(include %in% c("yes", "modify")) |>
  mutate(dataset_id = sprintf("%04d", as.integer(dataset_id)))

citations <- registry |>
  select(dataset_id, first_author, year) |>
  mutate(bib = map_chr(dataset_id, read_references),
         keys = map_chr(bib, entry_keys))

missing <- citations$dataset_id[is.na(citations$bib)]
if (length(missing) > 0)
  warning("no references in the openESM metadata for: ", paste(missing, collapse = ", "))

out_dir <- here("output", "paper")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
with_ref <- filter(citations, !is.na(bib))
# text outside @entries is ignored by biber, so the dataset lines only guide manual cleaning
writeLines(sprintf("%% %s, %s (%s)\n%s\n", with_ref$dataset_id, with_ref$first_author,
                   with_ref$year, with_ref$bib),
           here(out_dir, "dataset_references.bib"))
readr::write_tsv(select(citations, dataset_id, keys), here(out_dir, "dataset_citation_keys.tsv"))
message(sprintf("wrote %d references for %d of %d datasets to %s",
                sum(lengths(strsplit(na.omit(citations$keys), ","))), nrow(with_ref),
                nrow(citations), out_dir))
