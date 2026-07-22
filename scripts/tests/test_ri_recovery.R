# RI recovery test: generate data with known ICC, verify the pipeline recovers it.
# y_it = mu + u_i + e_it, with u_i ~ N(0, sigma_u), e_it ~ N(0, sigma_e).
# True ICC = sigma_u^2 / (sigma_u^2 + sigma_e^2).
#
# Two checks:
# 1. ICC from lmer variance components matches the true ICC
# 2. BLUPs correlate near-perfectly with the true person intercepts

library(here)
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "metrics.R"))

set.seed(42)
n_persons <- 200
n_obs <- 500
sigma_u <- 0.15
sigma_e <- 0.10
mu <- 0.5
true_icc <- sigma_u^2 / (sigma_u^2 + sigma_e^2)

person_intercepts <- rnorm(n_persons, 0, sigma_u)
rows <- list()
for (i in seq_len(n_persons)) {
  y <- mu + person_intercepts[i] + rnorm(n_obs, 0, sigma_e)
  rows[[i]] <- data.frame(
    id = sprintf("p%03d", i),
    beep = seq_len(n_obs),
    day = 1L,
    x1 = y,
    stringsAsFactors = FALSE
  )
}
df <- dplyr::bind_rows(rows)
features <- data.frame(name = "x1", answer_categories = "100", stringsAsFactors = FALSE)

cfg_test <- list(
  preprocess = list(
    standardize = "range01_scale_limits",
    lag_order = 1,
    lag_across_night = TRUE,
    lag_across_gaps = TRUE,
    missing = list(method = "none"),
    min_obs_person = 10
  ),
  cv = list(test_window = 10, warmup = 10, refit_per_origin = FALSE)
)

interim <- preprocess_dataset(df, features, cfg_test)

# fit the RI model on all valid data (same as in-sample fit)
all_valid <- lapply(interim$persons, function(p) subset_modeldata(p, which(p$valid)))
fitted <- ri_fit(all_valid, NULL)

# check 1: ICC from lmer variance components
vc <- as.data.frame(lme4::VarCorr(fitted$models[["x1"]]))
icc_hat <- vc$vcov[vc$grp == "id"] / sum(vc$vcov)

cat(sprintf("True ICC:          %.4f\n", true_icc))
cat(sprintf("Estimated ICC:     %.4f\n", icc_hat))
cat(sprintf("ICC difference:    %.4f\n", abs(icc_hat - true_icc)))
stopifnot(abs(icc_hat - true_icc) < 0.02)
cat("ICC from variance components: PASS\n")

# check 2: BLUPs recover true person intercepts
blup_df <- lme4::ranef(fitted$models[["x1"]])$id
blups <- setNames(blup_df[, 1], rownames(blup_df))
# match order: blups are named by id
true_u <- setNames(person_intercepts, sprintf("p%03d", seq_len(n_persons)))
shared <- intersect(names(blups), names(true_u))
r <- cor(blups[shared], true_u[shared])

cat(sprintf("BLUP-truth correlation: %.4f\n", r))
stopifnot(r > 0.99)
cat("BLUP recovery: PASS\n")
