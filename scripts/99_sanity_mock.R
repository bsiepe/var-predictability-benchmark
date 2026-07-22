# end-to-end ground-truth check on synthetic data.
# each person-item is AR(1) with known phi, so R2 = phi^2 by construction.
# checks: ar recovers mean(phi^2) in-sample; mean R2 = 0 in-sample; ar > mean OOS.

source("scripts/engine/config.R")
source("scripts/engine/mockdata.R")
source("scripts/engine/preprocess.R")
source("scripts/engine/models.R")
source("scripts/engine/crossval.R")
source("scripts/engine/metrics.R")
source("scripts/engine/run_dataset.R")

set.seed(cfg$seed)
mock <- make_mock_openesm(seed = 1)
interim <- preprocess_dataset(mock$data, mock$meta, cfg, dataset_id = "mock01")
result <- run_dataset(interim, cfg)

kept <- names(interim$persons)
truth_R2 <- mean(unlist(mock$truth[kept])^2)

met <- result$metrics
agg <- function(model, set) mean(met$R2[met$model == model & met$set == set], na.rm = TRUE)

cat(sprintf("\nPersons kept: %d / %d  (excluded: %s)\n",
            length(interim$persons), length(mock$truth),
            paste(interim$excluded, collapse = ", ")))
cat(sprintf("Mean phi^2 (ground truth) : %.3f\n", truth_R2))
cat(sprintf("ar    mean in-sample R2   : %.3f\n", agg("ar", "in")))
cat(sprintf("ar    mean OOS R2         : %.3f\n", agg("ar", "oos")))
cat(sprintf("trend mean in-sample R2   : %.3f\n", agg("trend", "in")))
cat(sprintf("trend mean OOS R2         : %.3f\n", agg("trend", "oos")))
cat(sprintf("mean  mean in-sample R2   : %.3f\n", agg("mean", "in")))
cat(sprintf("mean  mean OOS R2         : %.3f\n", agg("mean", "oos")))

ok <- abs(agg("ar", "in") - truth_R2) < 0.05 &&
      abs(agg("mean", "in")) < 0.02 &&
      agg("ar", "oos") > agg("mean", "oos") &&
      agg("trend", "in") >= agg("mean", "in") - 1e-10
cat(sprintf("\nSANITY %s\n", if (ok) "PASS" else "FAIL"))
if (!ok) quit(status = 1)
