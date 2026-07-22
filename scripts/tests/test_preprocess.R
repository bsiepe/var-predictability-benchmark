library(here)
source(here::here("scripts", "engine", "preprocess.R"))

pp <- list(
  standardize = "range01_scale_limits",
  lag_across_night = FALSE,
  lag_across_gaps = FALSE,
  missing = list(method = "none")
)

scale_bounds <- data.frame(name = "x1", scale_min = 0, scale_max = 4,
                           stringsAsFactors = FALSE)

cfg_test <- list(preprocess = c(pp, list(min_obs_person = 1L)))

# Tn == 1: no lag possible, so no valid timepoints
df_single <- data.frame(id = "p1", beep = 1L, day = 1L, x1 = 2)
result <- build_person(df_single, "x1", scale_bounds, pp)
stopifnot(nrow(result$Y) == 1L, !any(result$valid))
cat("Tn == 1: no valid timepoints: PASS\n")

# lag blocked at day boundary (row 3 crosses to new day)
df_days <- data.frame(
  id = "p1",
  day  = c(1L, 1L, 2L, 2L),
  beep = c(1L, 2L, 1L, 2L),
  x1   = c(1, 2, 3, 4)
)
result <- build_person(df_days, "x1", scale_bounds, pp)
stopifnot(identical(result$valid, c(FALSE, TRUE, FALSE, TRUE)))
cat("lag blocked at day boundary: PASS\n")

# lag blocked at beep gap (non-consecutive beep numbers within same day)
df_gap <- data.frame(
  id = "p1",
  day  = c(1L, 1L, 1L),
  beep = c(1L, 2L, 4L),
  x1   = c(1, 2, 3)
)
result <- build_person(df_gap, "x1", scale_bounds, pp)
stopifnot(identical(result$valid, c(FALSE, TRUE, FALSE)))
cat("lag blocked at beep gap: PASS\n")

# all-NA item column: preprocess_dataset must stop with informative error
features <- data.frame(name = "x1", answer_categories = "5", stringsAsFactors = FALSE)
df_all_na <- data.frame(id = rep("p1", 5), beep = 1:5, day = 1L, x1 = NA_real_)
caught <- tryCatch(preprocess_dataset(df_all_na, features, cfg_test), error = function(e) e)
stopifnot(inherits(caught, "error"), grepl("entirely NA", caught$message))
cat("all-NA item: error with informative message: PASS\n")

# n_cats < 2: preprocess_dataset must stop with informative error
features_bad <- data.frame(name = "x1", answer_categories = "1", stringsAsFactors = FALSE)
df_ok <- data.frame(id = rep("p1", 5), beep = 1:5, day = 1L, x1 = 0)
caught <- tryCatch(preprocess_dataset(df_ok, features_bad, cfg_test), error = function(e) e)
stopifnot(inherits(caught, "error"), grepl("n_cats < 2", caught$message))
cat("n_cats < 2: error with informative message: PASS\n")

cat("all preprocess tests passed\n")
