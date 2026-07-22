# ------------- Configuration for the benchmark engine -------------
# This file is the major configuration file for the benchmark

cfg <- list(
  seed = 30583,
  active_models = c("mean", "trend", "ar"),

  preprocess = list(
    standardize = "range01_scale_limits",
    lag_order = 1,
    lag_across_night = FALSE,
    lag_across_gaps = FALSE,
    missing = list(method = "none"),
    min_obs_person = 20
  ),

  cv = list(
    scheme = "holdout",
    test_window = 10,
    warmup = 10,
    refit_per_origin = TRUE
  ),

  metrics = list(
    primary = "stdRMSE",
    secondary = "R2"
  )
)
