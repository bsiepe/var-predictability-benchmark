# Recovery tests for multilevel AR and VAR models.
# Each section fits the model on synthetic data with known parameters and checks:
# (1) fixed-effect estimates recover the true population parameters
# (2) person-level BLUPs correlate with the true person-specific parameters

library(here)
source(here::here("scripts", "engine", "config.R"))
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


# ---------- ML-VAR(1) ----------
# Trivariate DGM with known Phi_pop (3x3) and person-specific deviations.
# N = 80 persons x n_t = 500 obs for reliable cross-lag recovery with 4 RE per equation.

set.seed(123)

N_v <- 80L
n_t_v <- 500L
vars_3 <- c("x1", "x2", "x3")
sd_delta <- 0.1
sd_innov_v <- 0.5
sd_alpha_v <- 0.3

# population VAR matrix: rows = outcome, cols = predictor (lag)
# includes one negative cross-lag (x3 -> x1) to test sign recovery
Phi_pop <- matrix(c( 0.5,  0.1, -0.1,
                     0.0,  0.4,  0.1,
                     0.1,  0.0,  0.6), nrow = 3, byrow = TRUE,
                  dimnames = list(vars_3, vars_3))

alpha_true_v <- matrix(rnorm(N_v * 3, 0, sd_alpha_v), nrow = N_v, ncol = 3)
# delta_true[i, row, col] is person i's deviation for Phi[row, col]
delta_true <- array(rnorm(N_v * 9, 0, sd_delta), dim = c(N_v, 3, 3))

persons_var <- lapply(seq_len(N_v), function(i) {
  Phi_i <- Phi_pop + delta_true[i, , ]
  alpha_i <- alpha_true_v[i, ]
  Y <- matrix(0, nrow = n_t_v, ncol = 3, dimnames = list(NULL, vars_3))
  for (t in 2:n_t_v) {
    Y[t, ] <- alpha_i + as.vector(Phi_i %*% Y[t - 1, ]) + rnorm(3, 0, sd_innov_v)
  }
  Ylag <- matrix(NA_real_, nrow = n_t_v, ncol = 3, dimnames = list(NULL, vars_3))
  Ylag[2:n_t_v, ] <- Y[1:(n_t_v - 1), , drop = FALSE]
  list(id = sprintf("q%03d", i), Y = Y, Ylag = Ylag, time = seq_len(n_t_v),
       valid = c(FALSE, rep(TRUE, n_t_v - 1)))
})

fitted_ml_var <- ml_var_fit(persons_var, spec = NULL)

# check 1: all 9 fixed-effect slopes within 0.05 of Phi_pop
all_ok <- TRUE
for (v in vars_3) {
  fe <- lme4::fixef(fitted_ml_var$models[[v]])
  v_idx <- which(vars_3 == v)
  for (w in vars_3) {
    lag_w <- paste0(w, "_lag")
    w_idx <- which(vars_3 == w)
    true_val <- Phi_pop[v_idx, w_idx]
    err <- abs(fe[lag_w] - true_val)
    cat(sprintf("ML-VAR  Phi[%s<-%s]: true=%.3f  est=%.3f  err=%.4f\n",
                v, w, true_val, fe[lag_w], err))
    if (err >= 0.05) all_ok <- FALSE
  }
}
stopifnot(all_ok)
cat("ML-VAR check 1 (fixed-effect slope recovery): PASS\n")

# check 2: BLUP slopes for x1->x1 (diagonal) correlate with true person-specific slopes
model_x1 <- fitted_ml_var$models[["x1"]]
fe_x1 <- lme4::fixef(model_x1)
re_x1 <- lme4::ranef(model_x1)$id
ids_v <- sprintf("q%03d", seq_len(N_v))
phi11_blup <- fe_x1["x1_lag"] + re_x1[ids_v, "x1_lag"]
phi11_true <- Phi_pop[1, 1] + delta_true[, 1, 1]
r_v <- cor(phi11_blup, phi11_true)
cat(sprintf("ML-VAR  BLUP slope correlation (x1->x1) with true phi_i: %.4f\n", r_v))
stopifnot(r_v > 0.80)
cat("ML-VAR check 2 (person-level BLUP slope recovery): PASS\n")
