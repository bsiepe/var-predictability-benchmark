# generates synthetic data matching the openESM format with known AR(1) coefficients.
# R2 = phi^2 by construction, giving the sanity check an interpretable criterion.
make_mock_openesm <- function(
  n_persons = 25,
  n_items = 3,
  beeps_per_day = 6,
  days = 8,
  scale = c(1, 7),
  phi_range = c(0.2, 0.7),
  innov_sd = 1.0,
  p_missing = 0.08,
  p_skip = 0.08,
  short_persons = 2,
  seed = 1
) {
  set.seed(seed)
  items <- paste0("x", seq_len(n_items))
  lo <- scale[1]
  hi <- scale[2]
  mid <- (lo + hi) / 2
  truth <- list()
  rows <- list()

  for (p in seq_len(n_persons)) {
    id <- sprintf("p%02d", p)
    p_days <- if (p <= short_persons) 2 else sample(max(3, days - 2):(days + 2), 1)
    n_t <- p_days * beeps_per_day
    phi <- runif(n_items, phi_range[1], phi_range[2])
    truth[[id]] <- setNames(phi, items)

    Y <- matrix(NA_real_, n_t, n_items, dimnames = list(NULL, items))
    for (j in seq_len(n_items)) {
      y <- numeric(n_t)
      y[1] <- rnorm(1, mid, innov_sd / sqrt(1 - phi[j]^2))
      for (t in 2:n_t) y[t] <- mid + phi[j] * (y[t - 1] - mid) + rnorm(1, 0, innov_sd)
      Y[, j] <- pmin(pmax(y, lo), hi)
    }

    day <- rep(seq_len(p_days), each = beeps_per_day)
    beep <- rep(seq_len(beeps_per_day), times = p_days)
    df <- data.frame(id = id, beep = beep, day = day, Y, stringsAsFactors = FALSE)
    for (j in items) df[[j]][runif(n_t) < p_missing] <- NA_real_
    df <- df[runif(n_t) >= p_skip, , drop = FALSE]
    rows[[id]] <- df
  }

  # meta mirrors the openESM features tibble: name + answer_categories
  list(
    data  = do.call(rbind, rows),
    meta  = data.frame(name = items,
                       answer_categories = as.character(hi - lo + 1L),
                       stringsAsFactors = FALSE),
    truth = truth
  )
}
