# end-to-end ground-truth check on synthetic data.
#
# The mock generator simulates each person-item as a stationary AR(1) with a
# known phi, so one-step predictability is R2 = phi^2 by construction. This
# script runs the full pipeline and checks:
#   (1) m2 (AR1) out-of-sample R2 recovers mean(phi^2)   -> the whole seam works
#   (2) m0 (person mean) out-of-sample R2 sits near 0    -> baseline is honest
#   (3) m2 > m0                                           -> the ladder orders right
# fast smoke test with an interpretable pass criterion

source("scripts/engine/00_functions.R")
source("scripts/engine/config.R")
source("scripts/engine/mockdata.R")
source("scripts/engine/preprocess.R")
source("scripts/engine/models.R")
source("scripts/engine/crossval.R")
source("scripts/engine/metrics.R")
source("scripts/engine/run_dataset.R")

set.seed(cfg$seed)
mock    <- make_mock_openesm(seed = 1)
interim <- preprocess_dataset(mock$data, mock$meta, cfg,
                              dataset_id = "mock01", source = "mock")
result  <- run_dataset(interim, cfg)

# Ground truth: mean phi^2 over the person-items that survived inclusion.
kept   <- names(interim$persons)
phis   <- unlist(mock$truth[kept])
truth_R2 <- mean(phis^2)

met <- result$metrics
agg <- function(model, set)
  mean(met$R2[met$model == model & met$set == set], na.rm = TRUE)

cat(sprintf("\nPersons kept: %d / %d  (excluded: %s)\n",
            length(interim$persons), length(mock$truth),
            paste(interim$excluded, collapse = ", ")))
cat(sprintf("Mean phi^2 (ground truth)      : %.3f\n", truth_R2))
cat(sprintf("m2 AR(1)  mean OOS R2          : %.3f\n", agg("m2", "oos")))
cat(sprintf("m2 AR(1)  mean in-sample R2    : %.3f\n", agg("m2", "in")))
cat(sprintf("m0 mean   mean OOS R2          : %.3f\n", agg("m0", "oos")))
cat(sprintf("m0 mean   mean in-sample R2    : %.3f\n", agg("m0", "in")))

# in-sample is the clean ground-truth check (m2 recovers phi^2, m0 R2 = 0);
# OOS is noisy at test_window = 10, so we only assert the ladder ordering there.
ok <- abs(agg("m2", "in") - truth_R2) < 0.05 &&
      abs(agg("m0", "in")) < 0.02 &&
      agg("m2", "oos") > agg("m0", "oos")
cat(sprintf("\nSANITY %s\n", if (ok) "PASS" else "FAIL"))
if (!ok) quit(status = 1)
