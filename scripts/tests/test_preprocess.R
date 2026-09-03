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

# --- lag-policy tests ---

pp_base <- list(standardize = "range01_scale_limits", lag_across_night = FALSE,
                lag_across_gaps = FALSE, use_consec_day = FALSE,
                missing = list(method = "none"))

# both day and beep entirely NA: no timing info → all consecutive pairs valid
df_na <- data.frame(id = "p1", day = NA_integer_, beep = NA_integer_,
                    x1 = c(1, 2, 3, 4))
result <- build_person(df_na, "x1", scale_bounds, pp_base)
stopifnot(identical(result$valid, c(FALSE, TRUE, TRUE, TRUE)))
cat("both day and beep all-NA: all consecutive valid: PASS\n")

# only day all-NA: same_day bypassed; consec_beep still enforced
df_naday <- data.frame(id = "p1", day = NA_integer_, beep = c(1L, 2L, 4L, 5L), x1 = 1:4)
result <- build_person(df_naday, "x1", scale_bounds, pp_base)
stopifnot(identical(result$valid, c(FALSE, TRUE, FALSE, TRUE)))
cat("only day all-NA: same_day bypassed, beep gap still blocked: PASS\n")

# only beep all-NA: consec_beep bypassed; same_day still enforced
df_nabeep <- data.frame(id = "p1", day = c(1L, 1L, 2L, 2L), beep = NA_integer_, x1 = 1:4)
result <- build_person(df_nabeep, "x1", scale_bounds, pp_base)
stopifnot(identical(result$valid, c(FALSE, TRUE, FALSE, TRUE)))
cat("only beep all-NA: consec_beep bypassed, day boundary still blocked: PASS\n")

# partial NA in day: those rows non-consecutive (NA → FALSE after lag_ok cleanup)
df_partial <- data.frame(id = "p1", day = c(1L, NA_integer_, 1L), beep = c(1L, 2L, 3L), x1 = 1:3)
result <- build_person(df_partial, "x1", scale_bounds, pp_base)
stopifnot(identical(result$valid, c(FALSE, FALSE, FALSE)))
cat("partial NA day: affected rows non-consecutive: PASS\n")

# daily mode: consecutive days valid; non-consecutive day (gap) blocked
pp_daily <- pp_base
pp_daily$lag_across_night <- TRUE
pp_daily$use_consec_day <- TRUE
df_daily <- data.frame(id = "p1", day = c(1L, 2L, 3L, 5L), beep = c(1L, 1L, 1L, 1L), x1 = 1:4)
result <- build_person(df_daily, "x1", scale_bounds, pp_daily)
stopifnot(identical(result$valid, c(FALSE, TRUE, TRUE, FALSE)))  # day 3→5 is a gap
cat("daily mode: consecutive days valid, day gap blocked: PASS\n")

cat("all preprocess tests passed\n")
