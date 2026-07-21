#-------- Run dataset with all models and compute metrics --------
# This is the main function that runs all active models on a dataset and computes performance metrics

run_dataset <- function(interim, cfg) {
  persons <- interim$persons
  stopifnot(length(persons) > 0)

  per_model <- lapply(cfg$active_models, function(m) {
    model <- model_registry[[m]]
    if (is.null(model)) stop("unknown model: ", m)
    # compute out-of-sample predictions for all persons in the dataset
    oos <- crossval_model(persons, model, cfg$cv)
    metrics <- compute_metrics(oos)$person
    metrics$model <- m
    metrics$label <- model$label
    list(metrics = metrics, oos = cbind(model = m, oos))
  })

  # ouptut structure
  list(
    dataset_id = interim$dataset_id,
    metrics = dplyr::bind_rows(purrr::map(per_model, "metrics")),
    oos = dplyr::bind_rows(purrr::map(per_model, "oos")),
    meta = list(
      settings = cfg,
      excluded = interim$excluded,
      n_person = length(persons)
    )
  )
}
