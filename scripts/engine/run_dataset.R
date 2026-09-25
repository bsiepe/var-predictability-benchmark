#-------- Run dataset with all models and compute metrics --------
# This is the main function that runs all active models on a dataset and computes performance metrics

# classify captured warnings and messages from model fitting.
# rank_deficient comes from the OLS models (ar, var), singular and convergence from lme4
.fit_issue_types <- c("rank_deficient", "singular", "convergence", "other")
.classify_fit_issue <- function(msg) {
  ifelse(grepl("rank-deficient OLS fit", msg), "rank_deficient",
         ifelse(grepl("isSingular", msg), "singular",
                ifelse(grepl("failed to converge", msg), "convergence", "other")))
}

# write to the log directly, so that the message handler below does not capture it again
.log_line <- function(...) cat(sprintf(...), "\n", sep = "", file = stderr())

run_dataset <- function(interim, cfg) {
  persons <- interim$persons
  stopifnot(length(persons) > 0)

  ml_var_uncorrelated <- "ml_var" %in% cfg$active_models && !is.null(cfg$ml_var.max_p) &&
    length(interim$items) > cfg$ml_var.max_p
  n_folds <- vapply(persons, function(p) length(make_folds(p, cfg$cv)), integer(1))
  # dataset-level models fit one lmer per item at each step: in-sample plus every OOS step
  n_fit_steps <- 1L + max(n_folds)
  # person-level models fit once in-sample plus once per fold, for every person
  n_person_fits <- sum(1L + n_folds)

  per_model <- lapply(cfg$active_models, function(m) {
    model <- model_registry[[m]]
    if (is.null(model)) stop("unknown model: ", m)
    message(sprintf("fitting %s...", m))

    spec <- cfg[[paste0(m, ".spec")]]
    if (m == "ml_var" && ml_var_uncorrelated) {
      spec$re_corr <- FALSE
      message(sprintf("[ml_var] p=%d > max_p=%d, using uncorrelated RE",
                      length(interim$items), cfg$ml_var.max_p))
    }

    # capture warnings, messages, and errors; continue on all
    issues <- character(0)
    result <- tryCatch(
      withCallingHandlers(
        {
          oos <- crossval_model(persons, model, cfg$cv, spec = spec)
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
          issues <<- c(issues, conditionMessage(w))
          .log_line("[%s] WARNING: %s", m, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(msg) {  # capture lme4 singular fit messages
          issues <<- c(issues, conditionMessage(msg))
          .log_line("[%s] %s", m, trimws(conditionMessage(msg)))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        .log_line("[%s] ERROR: %s", m, conditionMessage(e))
        list(metrics = data.frame(model = m, label = model$label)[0, ],
             metrics_var = data.frame(id = NA, variable = NA, set = NA,
                                      ss_res = NA, ss_tot = NA, n = NA,
                                      model = m)[0, ],
             oos = data.frame(id = NA, variable = NA, set = NA, t = NA_integer_,
                              yhat = NA, y = NA, model = m)[0, ],
             failed = TRUE)
      }
    )

    counts <- table(factor(.classify_fit_issue(issues), levels = .fit_issue_types))
    result$fit_issues <- data.frame(model = m, level = model$level, type = names(counts),
                                    n = as.integer(counts), stringsAsFactors = FALSE)
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
      n_valid = interim$n_valid[names(persons)],
      n_total = interim$n_total[names(persons)],
      n_imputed = interim$n_imputed[names(persons)],
      model_failures = stats::setNames(model_failures, cfg$active_models),
      # counts per model and type. denominators: n_person_fits for person-level models,
      # n_fit_steps * p (lmer calls) for dataset-level models
      fit_issues = dplyr::bind_rows(purrr::map(per_model, "fit_issues")),
      n_fit_steps = n_fit_steps,
      n_person_fits = n_person_fits,
      p = length(interim$items),
      ml_var_uncorrelated = ml_var_uncorrelated
    )
  )
}
