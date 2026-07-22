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
  if (method == "none") return(mat)
  stop(sprintf("missing$method '%s' not implemented", method))
}

# Building a person modeldata object from a single person's dataframe
build_person <- function(df_p, items, scale_bounds, pp) {
  # this function assumes that df_p is already ordered correctly per person
  Y <- as.matrix(df_p[, items, drop = FALSE])
  Y <- normalize_items(Y, scale_bounds, pp$standardize)
  Y <- apply_missing_policy(Y, pp$missing$method)
  Tn <- nrow(Y)

  day <- df_p$day
  beep <- df_p$beep
  # for each row t, does it share a day / immediately follow the previous beep?
  # (row 1 has no predecessor, so always FALSE)
  same_day <- c(FALSE, utils::tail(day, -1) == utils::head(day, -1))
  consec_beep <- c(FALSE, utils::tail(beep, -1) == utils::head(beep, -1) + 1L)
  night_ok <- pp$lag_across_night | same_day
  gap_ok <- pp$lag_across_gaps | consec_beep
  lag_ok <- night_ok & gap_ok

  # construct lagged Y matrix, with NA for first row and any rows that are not lag_ok
  Ylag <- matrix(NA_real_, nrow = Tn, ncol = ncol(Y), dimnames = dimnames(Y))
  if (Tn >= 2) Ylag[2:Tn, ] <- Y[1:(Tn - 1), , drop = FALSE]
  Ylag[!lag_ok, ] <- NA

  # check for complete cases in Y and Ylag, and mark valid observations
  valid <- lag_ok & stats::complete.cases(Y) & stats::complete.cases(Ylag)

  list(
    id = df_p$id[1],
    Y = Y,
    Ylag = Ylag,
    time = seq_len(Tn) - (Tn + 1) / 2,
    valid = valid
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
  keep <- n_valid >= pp$min_obs_person
  if (!any(keep)) warning("no person clears min_obs_person = ", pp$min_obs_person)

  list(
    dataset_id = dataset_id,
    persons = built[keep],
    excluded = names(built)[!keep],
    n_valid = n_valid,
    items = items,
    settings = pp
  )
}
