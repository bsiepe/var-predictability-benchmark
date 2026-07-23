library(here)
source(here("scripts/engine/models.R"))

# Trivariate VAR(1) with known Phi including two negative cross-lagged effects.
# With n=2000, OLS should recover all entries to within 0.05.
#
# Phi[j, k] = effect of variable j on variable k at the next timepoint.
# Negative entries: Phi[3,1] = -0.15 (x3 -> x1) and Phi[1,2] = -0.20 (x1 -> x2).
set.seed(42)
n <- 2000
vars <- c("x1", "x2", "x3")

Phi <- matrix(c(
   0.50,  0.20, -0.15,   # column 1: effects on x1
  -0.20,  0.40,  0.10,   # column 2: effects on x2
   0.10, -0.10,  0.30    # column 3: effects on x3
), nrow = 3, dimnames = list(vars, vars))

E <- matrix(rnorm(n * 3, 0, 0.5), n, 3, dimnames = list(NULL, vars))
Y <- matrix(0, n, 3, dimnames = list(NULL, vars))
for (t in 2:n) Y[t, ] <- Y[t - 1, ] %*% Phi + E[t, ]

Ylag <- rbind(matrix(NA_real_, 1, 3, dimnames = list(NULL, vars)), Y[-n, ])
train <- list(id = "p1", Y = Y, Ylag = Ylag, time = seq_len(n),
              valid = c(FALSE, rep(TRUE, n - 1)))

fitted <- var_fit(train, spec = NULL)
phi_est <- fitted$coefs[2:4, ]
dimnames(phi_est) <- dimnames(Phi)
max_err <- max(abs(phi_est - Phi))

cat("True Phi:\n")
print(round(Phi, 3))
cat("Estimated Phi:\n")
print(round(phi_est, 3))
cat(sprintf("Max recovery error: %.4f\n", max_err))
stopifnot(max_err < 0.05)
cat("VAR Phi recovery: PASS\n")

# prediction output has correct shape and no spurious NAs on valid rows
test_rows <- 2:n
test <- list(id = "p1", Y = Y[test_rows, ], Ylag = Ylag[test_rows, ],
             time = test_rows, valid = rep(TRUE, length(test_rows)))
Yhat <- var_predict(fitted, test)
stopifnot(identical(dim(Yhat), dim(test$Y)), all(is.finite(Yhat)))
cat("VAR prediction shape and finiteness: PASS\n")
