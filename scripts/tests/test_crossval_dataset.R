library(here)
source(here::here("scripts", "engine", "config.R"))
source(here::here("scripts", "engine", "mockdata.R"))
source(here::here("scripts", "engine", "preprocess.R"))
source(here::here("scripts", "engine", "models.R"))
source(here::here("scripts", "engine", "crossval.R"))
source(here::here("scripts", "engine", "metrics.R"))

# trivial dataset-level model: grand mean (ignores person identity)
gm_fit <- function(train_persons, spec) {
  all_Y <- do.call(rbind, lapply(train_persons, function(p) p$Y[p$valid, , drop = FALSE]))
  list(mu = colMeans(all_Y, na.rm = TRUE))
}
gm_predict <- function(fitted, test) {
  matrix(fitted$mu, nrow = nrow(test$Y), ncol = ncol(test$Y), byrow = TRUE,
         dimnames = dimnames(test$Y))
}
gm <- list(label = "Grand mean", level = "dataset", fit = gm_fit, predict = gm_predict)

set.seed(cfg$seed)
mock <- make_mock_openesm(seed = 1)
interim <- preprocess_dataset(mock$data, mock$meta, cfg, dataset_id = "mock01")
persons <- interim$persons

oos <- crossval_model(persons, gm, cfg$cv)
met <- compute_metrics(oos)$by_id

gm_in <- mean(met$R2[met$set == "in"])
gm_oos <- mean(met$R2[met$set == "oos"])
n_oos <- sum(oos$set == "oos")

cat(sprintf("Grand mean in-sample R2: %.3f\n", gm_in))
cat(sprintf("Grand mean OOS R2:       %.3f\n", gm_oos))
cat(sprintf("OOS rows: %d\n", n_oos))

# checks: OOS predictions exist, R2 is finite
stopifnot(n_oos > 0, is.finite(gm_in), is.finite(gm_oos))
cat("Dataset-level CV plumbing: PASS\n")

# ---------- Unit tests for crossval_dataset edge cases ----------

# minimal person structure: first row invalid (no lag), rest valid
make_person <- function(id, n, vars = c("x1")) {
  Y <- matrix(seq_len(n * length(vars)) * 0.1, nrow = n, ncol = length(vars),
              dimnames = list(NULL, vars))
  Ylag <- matrix(NA_real_, nrow = n, ncol = length(vars), dimnames = list(NULL, vars))
  if (n >= 2) Ylag[2:n, ] <- Y[1:(n - 1), , drop = FALSE]
  list(id = id, Y = Y, Ylag = Ylag, time = seq_len(n),
       valid = c(FALSE, rep(TRUE, n - 1)))
}

cv_unit <- list(test_window = 3, warmup = 3, refit_per_origin = TRUE)

# Test 1: person with 0 folds appears in in-sample but not OOS
{
  # p1: 6 valid tps → 3 folds; p2: 4 valid tps → 1 fold; p3: 3 valid tps → 0 folds
  ps <- list(make_person("p1", 7), make_person("p2", 5), make_person("p3", 4))
  out <- crossval_dataset(ps, gm, cv_unit)
  oos <- out[out$set == "oos", ]
  stopifnot(
    all(c("p1", "p2", "p3") %in% out$id),
    !("p3" %in% oos$id),
    sum(oos$id == "p1") == 3,
    sum(oos$id == "p2") == 1
  )
  cat("Edge case 1 (0-fold person excluded from OOS): PASS\n")
}

# Test 2: mixed fold lengths — exhausted person falls back to all_valid at later steps;
# OOS predictions generated only for persons with a fold at each step
{
  # p1: 3 folds (steps 1-3); p2: 1 fold (step 1 only, fallback at steps 2-3)
  ps <- list(make_person("p1", 7), make_person("p2", 5))
  out <- crossval_dataset(ps, gm, cv_unit)
  oos <- out[out$set == "oos", ]
  stopifnot(
    sum(oos$id == "p1") == 3,
    sum(oos$id == "p2") == 1
  )
  cat("Edge case 2 (mixed fold lengths, fallback exercised): PASS\n")
}

# Test 3: refit_per_origin = FALSE → n_steps = 1, multi-row test folds
{
  cv_no_refit <- list(test_window = 3, warmup = 3, refit_per_origin = FALSE)
  ps <- list(make_person("p1", 7), make_person("p2", 7))
  out <- crossval_dataset(ps, gm, cv_no_refit)
  oos <- out[out$set == "oos", ]
  stopifnot(
    sum(oos$id == "p1") == 3,
    sum(oos$id == "p2") == 3
  )
  cat("Edge case 3 (refit_per_origin = FALSE, multi-row test folds): PASS\n")
}

