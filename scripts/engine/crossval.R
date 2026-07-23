#------------- Cross-validation -------------
# This script contains the cross-validation logic

# Make folds for cross-validation
# person_data: a single person's data, including a logical vector `valid` indicating valid timepoints
# cv: a list with cross-validation parameters

make_folds <- function(person_data, cv) {
  valid_tps <- which(person_data$valid)
  tw <- min(cv$test_window, length(valid_tps) - cv$warmup)
  if (tw < 1) return(list())
  test_valid_tps <- utils::tail(valid_tps, tw)

  # if no refitting is done, we only need one fold with all valid training data and the test data
  if (!isTRUE(cv$refit_per_origin)) {
    return(list(list(train = valid_tps[valid_tps < min(test_valid_tps)], test = test_valid_tps)))
  }
  # rolling: one fold per origin, refit each time
  lapply(test_valid_tps, function(o) list(train = valid_tps[valid_tps < o], test = o))
}

# Create long-format dataframe with predictions and true values for a single person
# set: "in" for in-sample, "oos" for out-of-sample
# Yhat: matrix of predicted values (rows = timepoints, cols = variables)
# Y: matrix of true values (rows = timepoints, cols = variables)
.long_pairs <- function(id, set, Yhat, Y) {
  data.frame(
    id = id,
    variable = rep(colnames(Y), each = nrow(Y)),
    set = set,
    yhat = as.vector(Yhat),
    y = as.vector(Y),
    stringsAsFactors = FALSE
  )
}

# Perform cross-validation for a single person and a single model
crossval_person <- function(person_data, model, cv, spec = NULL) {
  # in-sample: fit and predict on all valid timepoints
  full <- subset_modeldata(person_data, which(person_data$valid))
  fitted <- model$fit(full, spec)
  out <- list(.long_pairs(person_data$id, "in", model$predict(fitted, full), full$Y))
  # OOS folds
  for (fold in make_folds(person_data, cv)) {
    train <- subset_modeldata(person_data, fold$train)
    test <- subset_modeldata(person_data, fold$test)
    fitted <- model$fit(train, spec)
    out[[length(out) + 1L]] <- .long_pairs(person_data$id, "oos", model$predict(fitted, test), test$Y)
  }
  dplyr::bind_rows(out)
}

# Dataset-level CV: model is fit on all persons jointly
# At each step k, all persons advance by one observation (aligned expanding windows).
# Persons who exhaust their test origins contribute all valid data at later steps
# (including former test observations)
crossval_dataset <- function(persons, model, cv, spec = NULL) {
  person_folds <- lapply(persons, make_folds, cv = cv)
  n_steps <- max(vapply(person_folds, length, integer(1)), 0L)
  all_valid <- lapply(persons, function(p) subset_modeldata(p, which(p$valid)))

  fit <- function(train_persons) model$fit(train_persons, spec)

  # in-sample
  fitted_in <- fit(all_valid)
  out <- purrr::map2(persons, all_valid, function(p, valid) {
    .long_pairs(p$id, "in", model$predict(fitted_in, valid), valid$Y)
  })

  # OOS: 1 step if refit_per_origin = FALSE, test_window steps otherwise
  for (k in seq_len(n_steps)) {
    train_at_k <- purrr::pmap(list(persons, person_folds, all_valid), function(p, folds, valid) {
      if (k <= length(folds)) subset_modeldata(p, folds[[k]]$train) else valid
    })
    fitted_k <- fit(train_at_k)
    for (i in seq_along(persons)) {
      folds_i <- person_folds[[i]]
      if (k <= length(folds_i)) {
        test <- subset_modeldata(persons[[i]], folds_i[[k]]$test)
        out[[length(out) + 1L]] <- .long_pairs(persons[[i]]$id, "oos",
                                                model$predict(fitted_k, test), test$Y)
      }
    }
  }
  dplyr::bind_rows(out)
}

# Wrapper to perform cross-validation for a single model across all persons in a dataset
crossval_model <- function(persons, model, cv, spec = NULL) {
  if (model$level == "person") {
    return(dplyr::bind_rows(lapply(persons, crossval_person, model = model, cv = cv, spec = spec)))
  }
  if (model$level == "dataset") {
    return(crossval_dataset(persons, model, cv, spec))
  }
  stop("unknown model level: ", model$level)
}
