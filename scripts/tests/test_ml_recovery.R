# Recovery tests for multilevel AR and VAR models.
# Each section fits the model on synthetic data with known parameters and checks:
# (1) fixed-effect estimates recover the true population parameters
# (2) person-level BLUPs correlate with the true person-specific parameters

library(here)
source(here::here("scripts", "engine", "models.R"))

set.seed(42)


# ---------- ML-AR(1) ----------
# True phi_i ~ N(mu_phi, sd_phi^2), alpha_i ~ N(0, sd_alpha^2).
# N = 100 persons x n_t = 200 obs gives reliable lmer estimates without singular fits.

N <- 100L
n_t <- 200L
mu_phi <- 0.5
sd_phi <- 0.15
sd_alpha <- 0.3
sd_innov <- 0.5

phi_true <- rnorm(N, mu_phi, sd_phi)
alpha_true <- rnorm(N, 0, sd_alpha)

persons <- lapply(seq_len(N), function(i) {
  y <- numeric(n_t)
  y[1] <- alpha_true[i] / (1 - phi_true[i])
  for (t in 2:n_t) y[t] <- alpha_true[i] + phi_true[i] * y[t - 1] + rnorm(1, 0, sd_innov)
  Y <- matrix(y, nrow = n_t, ncol = 1, dimnames = list(NULL, "x1"))
  Ylag <- matrix(NA_real_, nrow = n_t, ncol = 1, dimnames = list(NULL, "x1"))
  Ylag[2:n_t, ] <- Y[1:(n_t - 1), , drop = FALSE]
  list(id = sprintf("p%03d", i), Y = Y, Ylag = Ylag, time = seq_len(n_t),
       valid = c(FALSE, rep(TRUE, n_t - 1)))
})

fitted_ml_ar <- ml_ar_fit(persons, spec = NULL)
model_x1 <- fitted_ml_ar$models[["x1"]]

# check 1: fixed-effect slope ≈ mu_phi
phi_hat <- lme4::fixef(model_x1)[["x1_lag"]]
cat(sprintf("ML-AR  true mu_phi: %.3f  |  estimated: %.3f\n", mu_phi, phi_hat))
stopifnot(abs(phi_hat - mu_phi) < 0.05)
cat("ML-AR check 1 (fixed-effect slope recovery): PASS\n")

# check 2: BLUP slopes correlate with true person-specific phi_i
blup_df <- lme4::ranef(model_x1)$id
blup_slopes <- setNames(blup_df[, "x1_lag"], rownames(blup_df))
phi_blup <- phi_hat + blup_slopes[sprintf("p%03d", seq_len(N))]
r <- cor(phi_blup, phi_true)
cat(sprintf("ML-AR  BLUP slope correlation with true phi_i: %.4f\n", r))
stopifnot(r > 0.85)
cat("ML-AR check 2 (person-level BLUP slope recovery): PASS\n")
