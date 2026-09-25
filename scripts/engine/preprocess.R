#------------- Preprocess data for modeling -------------
# This script contains preprocessing functions for preparing the data for modeling

# normalizing items to a common scale
normalize_items <- function(mat, scale_bounds, method) {
  if (method == "range01_scale_limits") {
    for (v in colnames(mat)) {
      b <- scale_bounds[scale_bounds$name == v, ]
      mat[, v] <- (mat[, v] - b$scale_min) / (b$scale_max - b$scale_min)
    }
    return(mat)
  }
  stop(sprintf("standardize method '%s' not implemented", method))
}

# generic helper for applying missing data handling
apply_missing_policy <- function(mat, method) {
  if (method %in% c("none", "locf_lag")) return(mat)
  stop(sprintf("missing$method '%s' not implemented", method))
}

# impute missing values column-wise using imputeTS. only for lag construction, never for response Y.
impute_for_lag <- function(mat, method, max_consec) {
  if (is.null(max_consec)) max_consec <- Inf
  mat_orig <- mat
  for (v in seq_len(ncol(mat))) {
    if (!any(is.na(mat[, v])) || all(is.na(mat[, v]))) next
    mat[, v] <- switch(method,
      locf_lag = imputeTS::na_locf(mat[, v], maxgap = max_consec, na_remaining = "keep"),
      stop("unknown lag imputation method: ", method)
    )
  }
  list(mat = mat, n_imputed = sum(is.na(mat_orig) & !is.na(mat)))
}

# Building a person modeldata object from a single person's dataframe
build_person <- function(df_p, items, scale_bounds, pp) {
  # this function assumes that df_p is already ordered correctly per person
  Y <- as.matrix(df_p[, items, drop = FALSE])
  Y <- normalize_items(Y, scale_bounds, pp$standardize)
  Y <- apply_missing_policy(Y, pp$missing$method)  # returns Y unchanged for lag methods; imputation happens below
  Tn <- nrow(Y)

  day <- df_p$day
  beep <- df_p$beep
  # for each row t, does it share a day / immediately follow the previous beep?
  # (row 1 has no predecessor, so always FALSE)
  same_day <- c(FALSE, utils::tail(day, -1) == utils::head(day, -1))
  consec_beep <- c(FALSE, utils::tail(beep, -1) == utils::head(beep, -1) + 1L)
  consec_day <- c(FALSE, diff(day) == 1L)  # day[t] - day[t-1] == 1 (for daily diaries)

  # if a timing column is entirely absent, its constraint cannot be verified → unconstrained
  if (Tn >= 2 && all(is.na(day))) {
    # day column is entirely absent: both the night constraint (same_day) and the
    # daily-diary gap constraint (consec_day) cannot be verified, so treat all
    # rows as satisfying them.
    same_day[2:Tn] <- TRUE
    consec_day[2:Tn] <- TRUE
  }
  if (Tn >= 2 && all(is.na(beep))) consec_beep[2:Tn] <- TRUE

  consec <- if (isTRUE(pp$use_consec_day)) consec_day else consec_beep
  night_ok <- pp$lag_across_night | same_day
  gap_ok <- pp$lag_across_gaps | consec
  lag_ok <- night_ok & gap_ok
  lag_ok[is.na(lag_ok)] <- FALSE  # partial NA timing → treat as non-consecutive

  # impute Y for lag construction only; response Y stays original
  n_imputed <- 0L
  if (pp$missing$method == "locf_lag") {
    result <- impute_for_lag(Y, pp$missing$method, pp$missing$max_consec)
    Y_for_lag <- result$mat
    n_imputed <- result$n_imputed
  } else {
    Y_for_lag <- Y
  }

  Ylag <- matrix(NA_real_, nrow = Tn, ncol = ncol(Y), dimnames = dimnames(Y))
  if (Tn >= 2) Ylag[2:Tn, ] <- Y_for_lag[1:(Tn - 1), , drop = FALSE]
  Ylag[!lag_ok, ] <- NA

  valid <- lag_ok & stats::complete.cases(Y) & stats::complete.cases(Ylag)

  list(
    id = df_p$id[1],
    Y = Y,
    Ylag = Ylag,
    time = seq_len(Tn) - (Tn + 1) / 2,
    valid = valid,
    n_imputed = n_imputed
  )
}

# features: openESM features tibble with columns `name` and `answer_categories`.
# items are identified by non-empty answer_categories; scale bounds are inferred
# from the observed minimum + category count, computed globally across all persons.
# optional columns `scale_min` and `scale_max` declare bounds explicitly (e.g. for sum
# scores, where answer_categories describes the original items, not the aggregate).
# optional column `allow_outside_bounds` exempts items with declared bounds from the range
# guard (e.g. person-mean centred items, where only the width of the scale is known).
preprocess_dataset <- function(df, features, cfg, dataset_id = NA_character_) {
  pp <- cfg$preprocess
  has_categories <- features$answer_categories != ""
  items <- features$name[has_categories]
  n_cats <- as.integer(features$answer_categories[has_categories])

  if (!all(c("id", "beep", "day") %in% names(df)))
    stop("df missing required columns: ", paste(setdiff(c("id", "beep", "day"), names(df)), collapse = ", "))
  if (!all(items %in% names(df)))
    stop("df missing item columns: ", paste(setdiff(items, names(df)), collapse = ", "))

  declared <- rep(FALSE, length(items))
  decl_min <- rep(NA_real_, length(items))
  decl_max <- rep(NA_real_, length(items))
  if (all(c("scale_min", "scale_max") %in% names(features))) {
    decl_min <- features$scale_min[has_categories]
    decl_max <- features$scale_max[has_categories]
    declared <- !is.na(decl_min) & !is.na(decl_max)
    bad <- declared & decl_max <= decl_min
    if (any(bad)) stop("declared scale_max <= scale_min for: ", paste(items[bad], collapse = ", "))
  }
  allow_outside <- rep(FALSE, length(items))
  if ("allow_outside_bounds" %in% names(features)) {
    allow_outside <- features$allow_outside_bounds[has_categories] %in% TRUE
    if (any(allow_outside & !declared))
      stop("allow_outside_bounds requires declared scale_min and scale_max for: ",
           paste(items[allow_outside & !declared], collapse = ", "))
  }
  if (any(n_cats[!declared] < 2))
    stop("items with n_cats < 2: ", paste(items[!declared][n_cats[!declared] < 2], collapse = ", "))

  obs_min <- vapply(items, function(v) min(df[[v]], na.rm = TRUE), numeric(1))
  obs_max <- vapply(items, function(v) max(df[[v]], na.rm = TRUE), numeric(1))
  if (any(!is.finite(obs_min)))
    stop("items are entirely NA: ", paste(items[!is.finite(obs_min)], collapse = ", "))

  scale_min <- floor(obs_min)
  scale_max <- scale_min + n_cats - 1L
  scale_min[declared] <- decl_min[declared]
  scale_max[declared] <- decl_max[declared]

  # every observed value must lie within its bounds, so that all items normalise into [0, 1]
  tol <- 1e-8 * (scale_max - scale_min)
  outside <- (obs_min < scale_min - tol | obs_max > scale_max + tol) & !allow_outside
  if (any(outside))
    stop("dataset ", dataset_id, ": values outside scale bounds for\n",
         paste(sprintf("  %s: observed %g to %g, bounds %g to %g", items[outside],
                       obs_min[outside], obs_max[outside], scale_min[outside], scale_max[outside]),
               collapse = "\n"))

  scale_bounds <- data.frame(name = items, scale_min = unname(scale_min),
                             scale_max = unname(scale_max),
                             source = ifelse(declared, "declared", "derived"),
                             allow_outside = allow_outside,
                             stringsAsFactors = FALSE)

  built <- lapply(split(df, df$id), build_person,
                  items = items, scale_bounds = scale_bounds, pp = pp)

  n_valid <- vapply(built, function(person_data) sum(person_data$valid), integer(1))
  n_total <- vapply(built, function(person_data) nrow(person_data$Y), integer(1))
  n_imputed <- vapply(built, function(person_data) person_data$n_imputed, integer(1))
  stopifnot(identical(names(n_valid), names(n_total)))

  # require all items to have nonzero variance across valid observations
  has_variance <- vapply(built, function(person_data) {
    v <- person_data$valid
    if (sum(v) < 2L) return(FALSE)
    all(apply(person_data$Y[v, , drop = FALSE], 2, stats::var, na.rm = TRUE) > 0)
  }, logical(1))

  too_few <- n_valid < pp$min_obs_person
  no_var <- !too_few & !has_variance
  keep <- !too_few & has_variance
  if (!any(keep)) warning("no person retained (", sum(too_few), " too few obs, ",
                          sum(no_var), " zero variance)")

  excluded <- data.frame(
    id = names(built)[!keep],
    reason = ifelse(too_few[!keep], "low_obs", "zero_var"),
    stringsAsFactors = FALSE
  )

  list(
    dataset_id = dataset_id,
    persons = built[keep],
    excluded = excluded,
    n_valid = n_valid,
    n_total = n_total,
    n_imputed = n_imputed,
    has_variance = has_variance,
    items = items,
    scale_bounds = scale_bounds,
    settings = pp
  )
}
