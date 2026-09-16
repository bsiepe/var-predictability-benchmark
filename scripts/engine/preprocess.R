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
  if (method %in% c("none", "locf_lag", "kalman_lag")) return(mat)
  stop(sprintf("missing$method '%s' not implemented", method))
}

# impute missing values column-wise using imputeTS. only for lag construction, never for response Y.
impute_for_lag <- function(mat, method, max_consec) {
  mat_orig <- mat
  for (v in seq_len(ncol(mat))) {
    if (!any(is.na(mat[, v]))) next
    mat[, v] <- switch(method,
      locf_lag = imputeTS::na_locf(mat[, v], maxgap = max_consec),
      kalman_lag = imputeTS::na_kalman(mat[, v], smooth = FALSE, maxgap = max_consec),
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
  if (Tn >= 2 && all(is.na(day)))  same_day[2:Tn]    <- TRUE
  if (Tn >= 2 && all(is.na(beep))) consec_beep[2:Tn] <- TRUE

  consec <- if (isTRUE(pp$use_consec_day)) consec_day else consec_beep
  night_ok <- pp$lag_across_night | same_day
  gap_ok <- pp$lag_across_gaps | consec
  lag_ok <- night_ok & gap_ok
  lag_ok[is.na(lag_ok)] <- FALSE  # partial NA timing → treat as non-consecutive

  # impute Y for lag construction only; response Y stays original
  n_imputed <- 0L
  if (pp$missing$method %in% c("locf_lag", "kalman_lag")) {
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
preprocess_dataset <- function(df, features, cfg, dataset_id = NA_character_) {
  pp <- cfg$preprocess
  has_categories <- features$answer_categories != ""
  items <- features$name[has_categories]
  n_cats <- as.integer(features$answer_categories[has_categories])

  if (!all(c("id", "beep", "day") %in% names(df)))
    stop("df missing required columns: ", paste(setdiff(c("id", "beep", "day"), names(df)), collapse = ", "))
  if (!all(items %in% names(df)))
    stop("df missing item columns: ", paste(setdiff(items, names(df)), collapse = ", "))
  if (any(n_cats < 2)) stop("items with n_cats < 2: ", paste(items[n_cats < 2], collapse = ", "))

  scale_min <- vapply(items, function(v) floor(min(df[[v]], na.rm = TRUE)), numeric(1))
  if (any(!is.finite(scale_min)))
    stop("items are entirely NA: ", paste(items[!is.finite(scale_min)], collapse = ", "))

  scale_bounds <- data.frame(name = items, scale_min = scale_min,
                             scale_max = scale_min + n_cats - 1L,
                             stringsAsFactors = FALSE)

  built <- lapply(split(df, df$id), build_person,
                  items = items, scale_bounds = scale_bounds, pp = pp)

  n_valid <- vapply(built, function(person_data) sum(person_data$valid), integer(1))
  n_imputed <- vapply(built, function(person_data) person_data$n_imputed, integer(1))
  keep <- n_valid >= pp$min_obs_person
  if (!any(keep)) warning("no person clears min_obs_person = ", pp$min_obs_person)

  list(
    dataset_id = dataset_id,
    persons = built[keep],
    excluded = names(built)[!keep],
    n_valid = n_valid,
    n_imputed = n_imputed,
    items = items,
    settings = pp
  )
}
