library(here)
source(here::here("scripts", "engine", "crossval.R"))

#---- Test cross-validation fold generation ----
assert_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(
      sprintf(
        "%s\nexpected: %s\nactual:   %s",
        label,
        paste(capture.output(str(expected)), collapse = " "),
        paste(capture.output(str(actual)), collapse = " ")
      ),
      call. = FALSE
    )
  }
}

md <- list(valid = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))

assert_identical(
  make_folds(md, list(test_window = 3L, warmup = 4L, refit_per_origin = TRUE)),
  list(),
  "make_folds() should return no folds when warmup consumes all valid rows"
)

assert_identical(
  make_folds(md, list(test_window = 10L, warmup = 1L, refit_per_origin = TRUE)),
  list(
    list(train = c(2L), test = 4L),
    list(train = c(2L, 4L), test = 5L),
    list(train = c(2L, 4L, 5L), test = 7L)
  ),
  "make_folds() rolling: train contains only valid positions, test_window clamped"
)

assert_identical(
  make_folds(md, list(test_window = 2L, warmup = 2L, refit_per_origin = FALSE)),
  list(list(train = c(2L, 4L), test = c(5L, 7L))),
  "make_folds() holdout: train contains only valid positions"
)

# critical: no invalid row index leaks into any train set
folds <- make_folds(md, list(test_window = 10L, warmup = 1L, refit_per_origin = TRUE))
vpos <- which(md$valid)
for (fold in folds) {
  if (!all(fold$train %in% vpos))
    stop("train indices contain invalid rows", call. = FALSE)
}
cat("no invalid rows in any train set: PASS\n")

cat("crossval fold tests passed\n")
