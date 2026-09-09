#-------- Run dataset with all models and compute metrics --------
# This is the main function that runs all active models on a dataset and computes performance metrics

run_dataset <- function(interim, cfg) {
  persons <- interim$persons
  stopifnot(length(persons) > 0)

  per_model <- lapply(cfg$active_models, function(m) {
    model <- model_registry[[m]]
    if (is.null(model)) stop("unknown model: ", m)
    message(sprintf("fitting %s...", m))

    # capture warnings, messages, and errors; continue on all
    result <- tryCatch(
      withCallingHandlers(
        {
          oos <- crossval_model(persons, model, cfg$cv,
                               spec = cfg[[paste0(m, ".spec")]])
          m_out <- compute_metrics(oos)
          metrics <- m_out$by_id
          metrics$model <- m
          metrics$label <- model$label
          metrics_var <- m_out$by_id_variable
          metrics_var$model <- m
          list(metrics = metrics, metrics_var = metrics_var,
               oos = cbind(model = m, oos), failed = FALSE)
        },
        warning = function(w) {
          message(sprintf("[%s] WARNING: %s", m, conditionMessage(w)))
          invokeRestart("muffleWarning")
        },
        message = function(msg) {  # capture lme4 singular fit messages
          message(sprintf("[%s] %s", m, conditionMessage(msg)))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        message(sprintf("[%s] ERROR: %s", m, conditionMessage(e)))
        list(metrics = data.frame(model = m, label = model$label)[0, ],
             metrics_var = data.frame(id = NA, variable = NA, set = NA,
                                      ss_res = NA, ss_tot = NA, n = NA,
                                      model = m)[0, ],
             oos = data.frame(id = NA, variable = NA, set = NA,
                              yhat = NA, y = NA, model = m)[0, ],
             failed = TRUE)
      }
    )

    result
  })

  effective_cfg <- cfg
  effective_cfg$preprocess <- interim$settings

  # collect failure status for each model
  model_failures <- vapply(per_model, function(x) x$failed, logical(1))

  list(
    dataset_id = interim$dataset_id,
    metrics = dplyr::bind_rows(purrr::map(per_model, "metrics")),
    metrics_var = dplyr::bind_rows(purrr::map(per_model, "metrics_var")),
    oos = dplyr::bind_rows(purrr::map(per_model, "oos")),
    meta = list(
      settings = effective_cfg,
      excluded = interim$excluded,
      n_person = length(persons),
      model_failures = stats::setNames(model_failures, cfg$active_models)
    )
  )
}
