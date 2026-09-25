library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "metrics.R"))
source(here::here("scripts", "engine", "run_dataset.R"))

set.seed(cfg$seed)
mock <- make_mock_openesm(seed = 1)
interim <- preprocess_dataset(mock$data, mock$meta, cfg, dataset_id = "mock01")

cfg_small <- cfg
cfg_small$active_models <- c("mean", "ar", "var", "ri", "ml_ar", "ml_var")
cfg_small$ml_var.max_p <- 1  # mock has 3 items, so ml_var falls back to uncorrelated RE
result <- suppressMessages(run_dataset(interim, cfg_small))
issues <- result$meta$fit_issues

# 1. one row per model and issue type, with non-negative integer counts and the model level
stopifnot(nrow(issues) == length(cfg_small$active_models) * length(.fit_issue_types),
          setequal(issues$type, .fit_issue_types),
          is.integer(issues$n), all(issues$n >= 0),
          all(issues$level[issues$model %in% c("mean", "ar", "var")] == "person"),
          all(issues$level[issues$model %in% c("ri", "ml_ar", "ml_var")] == "dataset"))
cat("fit_issues: one row per model and type: PASS\n")

# 2. issue types stay with their model family
person <- issues$level == "person"
stopifnot(all(issues$n[person & issues$type %in% c("singular", "convergence")] == 0),
          all(issues$n[!person & issues$type == "rank_deficient"] == 0),
          sum(issues$n[!person & issues$type == "singular"]) > 0)
cat("fit_issues: types separated by model family: PASS\n")

# 3. denominators: lmer calls for dataset-level models, fit calls for person-level models
n_folds <- vapply(interim$persons, function(p) length(make_folds(p, cfg$cv)), integer(1))
stopifnot(result$meta$n_fit_steps == 1 + max(n_folds),
          result$meta$n_person_fits == sum(1 + n_folds),
          result$meta$p == length(interim$items),
          all(issues$n[!person & issues$type == "singular"] <=
                result$meta$n_fit_steps * result$meta$p),
          all(issues$n[person & issues$type == "rank_deficient"] <= result$meta$n_person_fits))
cat("fit_issues: denominators consistent with counts: PASS\n")

# 4. ml_var fallback is recorded
stopifnot(isTRUE(result$meta$ml_var_uncorrelated))
cfg_small$ml_var.max_p <- 12
result_corr <- suppressMessages(run_dataset(interim, cfg_small))
stopifnot(isFALSE(result_corr$meta$ml_var_uncorrelated))
cat("ml_var_uncorrelated flag recorded: PASS\n")

cat("all run_dataset tests passed\n")
