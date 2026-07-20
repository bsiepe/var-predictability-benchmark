# 04_meta_regression.R — aggregate per-dataset results and run the meta-regression.
#
# Steps:
#   1. read all output/results/*.rds into one person-level table
#   2. fit the meta-regression: individual- and study-level moderators on
#      person-level performance metrics, precision-weighted by test-set size (n_test)
#   3. saveRDS combined table + fitted objects to output/meta/
#
