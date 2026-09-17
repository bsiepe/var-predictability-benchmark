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

# --- LOCF lag imputation tests ---

pp_locf <- pp_base
pp_locf$missing <- list(method = "locf_lag", max_consec = 2)
pp_locf$lag_across_night <- TRUE
pp_locf$lag_across_gaps <- TRUE

# 1. single-item NA: Y[t] stays NA, Ylag[t+1] filled, row t+1 becomes valid
df_locf1 <- data.frame(id = "p1", day = 1L, beep = 1:4, x1 = c(1, 2, NA, 4))
result <- build_person(df_locf1, "x1", scale_bounds, pp_locf)
stopifnot(is.na(result$Y[3, "x1"]))              # response preserved
stopifnot(!is.na(result$Ylag[4, "x1"]))           # lag filled by LOCF
stopifnot(result$valid[3] == FALSE)                # row 3 invalid (Y NA)
stopifnot(result$valid[4] == TRUE)                 # row 4 now valid
result_none <- build_person(df_locf1, "x1", scale_bounds, pp_base)
stopifnot(result_none$valid[4] == FALSE)           # without LOCF, row 4 would be invalid
cat("LOCF: single-item NA fills Ylag but not Y: PASS\n")

# 2. maxgap guard: gap of 3 with maxgap=2 → none filled; gap of 2 → all filled
df_locf2 <- data.frame(id = "p1", day = 1L, beep = 1:8,
                        x1 = c(1, NA, NA, NA, 4, NA, NA, 4))
result <- build_person(df_locf2, "x1", scale_bounds, pp_locf)
# gap of 3 (t=2,3,4): exceeds maxgap=2, all left NA
stopifnot(is.na(result$Ylag[3, "x1"]))
stopifnot(is.na(result$Ylag[5, "x1"]))
# gap of 2 (t=6,7): within maxgap, both filled
stopifnot(!is.na(result$Ylag[7, "x1"]))
stopifnot(!is.na(result$Ylag[8, "x1"]))
cat("LOCF: maxgap guard: long gap unfilled, short gap filled: PASS\n")

# 3. n_imputed count (only the gap-of-2 was filled)
stopifnot(result$n_imputed == 2L)
cat("LOCF: n_imputed count correct: PASS\n")

# 4. backward compatibility: method="none" → n_imputed=0
result_compat <- build_person(df_locf1, "x1", scale_bounds, pp_base)
stopifnot(result_compat$n_imputed == 0L)
cat("LOCF: method=none returns n_imputed=0: PASS\n")

# 5. lag_ok boundaries still enforced after LOCF
pp_locf_day <- pp_locf
pp_locf_day$lag_across_night <- FALSE
df_locf3 <- data.frame(id = "p1", day = c(1L, 1L, 2L, 2L),
                        beep = c(1L, 2L, 1L, 2L),
                        x1 = c(1, 2, NA, 4))
result <- build_person(df_locf3, "x1", scale_bounds, pp_locf_day)
stopifnot(is.na(result$Ylag[3, "x1"]))            # lag_ok=FALSE at day boundary
stopifnot(result$valid[3] == FALSE)
stopifnot(!is.na(result$Ylag[4, "x1"]))           # within day, LOCF fills
stopifnot(result$valid[4] == TRUE)
cat("LOCF: lag_ok boundaries still enforced: PASS\n")

# 6. multi-item: one item NA doesn't block another
scale_bounds2 <- data.frame(name = c("x1", "x2"), scale_min = c(0, 0),
                             scale_max = c(4, 4), stringsAsFactors = FALSE)
df_multi <- data.frame(id = "p1", day = 1L, beep = 1:4,
                        x1 = c(1, NA, 3, 4), x2 = c(1, 2, 3, 4))
result <- build_person(df_multi, c("x1", "x2"), scale_bounds2, pp_locf)
stopifnot(result$valid[2] == FALSE)                # Y[2, x1] NA → row 2 invalid
stopifnot(result$valid[3] == TRUE)                 # Ylag[3, x1] filled by LOCF
stopifnot(abs(result$Ylag[3, "x1"] - 0.25) < 1e-10)  # normalized(1) = 1/4
cat("LOCF: multi-item single NA doesn't block other items: PASS\n")

# 7. leading NA stays NA (no backward fill)
df_lead <- data.frame(id = "p1", day = 1L, beep = 1:4, x1 = c(NA, 2, 3, 4))
result <- build_person(df_lead, "x1", scale_bounds, pp_locf)
stopifnot(is.na(result$Y[1, "x1"]))               # response NA preserved
stopifnot(is.na(result$Ylag[2, "x1"]))            # Ylag[2] = Y_for_lag[1], which should still be NA
cat("LOCF: leading NA stays NA (no backward fill): PASS\n")

# 8. all-NA column for one person doesn't crash
scale_bounds2 <- data.frame(name = c("x1", "x2"), scale_min = c(0, 0),
                             scale_max = c(4, 4), stringsAsFactors = FALSE)
df_allna <- data.frame(id = "p1", day = 1L, beep = 1:4,
                        x1 = c(NA, NA, NA, NA), x2 = c(1, 2, 3, 4))
result <- build_person(df_allna, c("x1", "x2"), scale_bounds2, pp_locf)
stopifnot(all(is.na(result$Y[, "x1"])))           # all-NA column unchanged
stopifnot(result$n_imputed == 0L)                  # nothing to impute
cat("LOCF: all-NA column doesn't crash: PASS\n")

# --- zero-variance person exclusion tests ---

# 1. one constant item, one varying: person excluded (all items must have variance)
features2 <- data.frame(name = c("x1", "x2"), answer_categories = c("5", "5"),
                         stringsAsFactors = FALSE)
cfg_var <- list(preprocess = c(pp_base, list(min_obs_person = 2L,
                lag_across_night = TRUE, lag_across_gaps = TRUE)))
df_const <- data.frame(id = rep("p1", 5), beep = 1:5, day = 1L,
                        x1 = c(2, 2, 2, 2, 2), x2 = c(1, 2, 3, 4, 0))
res <- preprocess_dataset(df_const, features2, cfg_var, dataset_id = "test")
stopifnot(nrow(res$excluded) == 1L, res$excluded$reason == "zero_var")
stopifnot(length(res$persons) == 0L)
cat("zero-var: constant item excludes person: PASS\n")

# 2. variance on all items: person retained
df_vary <- data.frame(id = rep("p1", 5), beep = 1:5, day = 1L,
                       x1 = c(0, 1, 2, 3, 4), x2 = c(1, 2, 3, 4, 0))
res <- preprocess_dataset(df_vary, features2, cfg_var, dataset_id = "test")
stopifnot(length(res$persons) == 1L, nrow(res$excluded) == 0L)
cat("zero-var: varying items retained: PASS\n")

# 3. variance in invalid rows only: excluded
cfg_strict <- list(preprocess = c(pp_base, list(min_obs_person = 1L)))
df_inv <- data.frame(id = rep("p1", 4), day = c(1L, 2L, 2L, 2L),
                      beep = c(1L, 1L, 2L, 3L), x1 = c(99, 2, 2, 2))
res <- preprocess_dataset(df_inv, features, cfg_strict, dataset_id = "test")
stopifnot(nrow(res$excluded) == 1L, res$excluded$reason == "zero_var")
cat("zero-var: variance only in invalid rows: excluded: PASS\n")

# 4. too few obs caught before variance check (reason = low_obs)
df_tiny <- data.frame(id = rep("p1", 2), beep = 1:2, day = 1L, x1 = c(1, 2))
cfg_high <- list(preprocess = c(pp_base, list(min_obs_person = 99L,
                  lag_across_night = TRUE, lag_across_gaps = TRUE)))
res <- preprocess_dataset(df_tiny, features, cfg_high, dataset_id = "test")
stopifnot(nrow(res$excluded) == 1L, res$excluded$reason == "low_obs")
cat("zero-var: too few obs -> reason is low_obs: PASS\n")

cat("all preprocess tests passed\n")
