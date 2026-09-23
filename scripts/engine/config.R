# ------------- Configuration for the benchmark engine -------------
# This file is the major configuration file for the benchmark

cfg <- list(
  seed = 30583,
  active_models = c("mean", "locf", "trend", "ri", "ar", "var", "ml_ar", "ml_var"),
  ml_var.spec = list(re_corr = TRUE),   # re_corr = FALSE → uncorrelated (||) for robustness
  ml_var.max_p = 12,  # if p > this, fall back to uncorrelated RE (re_corr = FALSE)

  preprocess = list(
    standardize = "range01_scale_limits",
    lag_order = 1,
    lag_across_night = FALSE,
    lag_across_gaps = FALSE,
    use_consec_day = FALSE,   # if TRUE, gap check uses diff(day)==1 instead of consec beep (daily diaries)
    missing = list(method = "locf_lag", max_consec = 2), # "locf_lag": LOCF impute for lag construction only
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
