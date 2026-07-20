# synthetic openESM-format datasets with known ground truth.
#
# Deliberately exercises the fragile paths: unequal series lengths, a couple of
# persons below the inclusion floor, overnight gaps, skipped beeps, and NA values.

make_mock_openesm <- function(
  n_persons = 25,
  n_items  = 3,
  beeps_per_day = 6,
  days  = 8,               # nominal length; per-person length varies below
  scale = c(1, 7),         # response bounds (Likert-like)
  phi_range   = c(0.2, 0.7),     # per person-item AR(1) coefficient drawn here
  innov_sd = 1.0,             # innovation SD on the latent scale
  p_missing   = 0.08,            # fraction of responses set NA (item-wise)
  p_skip  = 0.08,            # fraction of scheduled beeps with no row at all
  short_persons = 2,             # this many persons get a too-short series (< floor)
  seed = 1
) {
  set.seed(seed)
  items <- paste0("x", seq_len(n_items))
  lo <- scale[1]; hi <- scale[2]; mid <- (lo + hi) / 2

  truth <- list()   # store true phi per person-item for the sanity check
  rows  <- list()

  for (p in seq_len(n_persons)) {
    id <- sprintf("p%02d", p)

    # Vary length; make the first `short_persons` deliberately too short.
    p_days <- if (p <= short_persons) 2 else sample(max(3, days - 2):(days + 2), 1)
    n_t    <- p_days * beeps_per_day

    phi <- runif(n_items, phi_range[1], phi_range[2])
    truth[[id]] <- setNames(phi, items)

    # Simulate each item as AR(1) around the scale midpoint on the latent scale,
    # then clip to the response bounds (realistic; phi is linear-transform
    # invariant so R2 = phi^2 survives the clip approximately).
    Y <- matrix(NA_real_, n_t, n_items, dimnames = list(NULL, items))
    for (j in seq_len(n_items)) {
      y <- numeric(n_t)
      y[1] <- rnorm(1, mid, innov_sd / sqrt(1 - phi[j]^2))
      for (t in 2:n_t) y[t] <- mid + phi[j] * (y[t - 1] - mid) + rnorm(1, 0, innov_sd)
      Y[, j] <- pmin(pmax(y, lo), hi)
    }

    day  <- rep(seq_len(p_days), each = beeps_per_day)
    beep <- rep(seq_len(beeps_per_day), times = p_days)

    df <- data.frame(id = id, beep = beep, day = day, Y, stringsAsFactors = FALSE)

    # Item-wise missingness (values present as NA).
    for (j in items) df[[j]][runif(n_t) < p_missing] <- NA_real_
    # Skipped beeps (whole rows absent) -> creates beep-gaps to test lag masking.
    df <- df[runif(n_t) >= p_skip, , drop = FALSE]

    rows[[id]] <- df
  }

  list(
    data  = do.call(rbind, rows),
    meta  = data.frame(item = items, scale_min = lo, scale_max = hi,
                       stringsAsFactors = FALSE),
    truth = truth   # not available for real data; used only by sanity checks
  )
}
