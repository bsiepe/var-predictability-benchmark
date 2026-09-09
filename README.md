# VAR Predictability Benchmark

Empirical assessment of the predictive performance of multilevel VAR models across a
large collection of ESM datasets.

---

## Design in one paragraph

Per-dataset fitting writes a cached result file (`output/results/<id>.rds`); reports only
read caches, never refit. `make` tracks file-level dependencies and parallelises with
`make -jN`. Modularity comes from isolating the four things most likely to change:

| Seam | What changes behind it | Touch only |
|---|---|---|
| **Data harmonisation** | adding a (non-openESM) dataset | `scripts/engine/harmonize.R` + raw loader |
| **Model registry** | adding/altering a ladder rung | `scripts/engine/models.R` |
| **Cross-validation** | the CV scheme (e.g. block → rolling) | `scripts/engine/crossval.R` (`make_folds`) |
| **Metrics** | R² / RMSE definitions | `scripts/engine/metrics.R` |

---

## Pipeline

```
data/raw/<id>            --preprocess_one.R-->  data/interim/<id>.rds   (standardized modeldata)
data/interim/<id>.rds    --fit_one.R-------->   output/results/<id>.rds (per-person OOS predictions + metrics)
output/results/*.rds     --04_meta_regression.R-> output/meta/          (combined results, meta-regression)
output/{results,meta}    --05_results.qmd ----> figures/ + tables       (reports; read-only)
```

Dataset IDs are derived from the openESM metadata table in `data/meta/`, not a
hand-maintained manifest. Make generates one target per dataset from that list.

---

## Directory layout

```
scripts/            all code
  engine/           sourced, never run top-to-bottom
    00_functions.R  generic helpers, package loading
    config.R        single settings list: CV scheme + params, seeds, active model set
    harmonize.R     Blanchard-audited datasets -> openESM conventions
    preprocess.R    standardized data -> per-person `modeldata` (lags, standardization, masking)
    models.R        all ladder rungs + registry (the fit/predict interface)
    crossval.R      make_folds (the swappable scheme) + fold runner + OOS table
    metrics.R       person-level R² (primary), standardized RMSE (secondary)
    mockdata.R      synthetic openESM-format data with known ground truth
    run_dataset.R   orchestrates one dataset end-to-end
  preprocess_one.R  CLI: <dataset_id>  (Make target)
  fit_one.R         CLI: <dataset_id>  (expensive; Make target)
  99_sanity_mock.R  end-to-end ground-truth check on mock data
  02_descriptives.qmd
  04_meta_regression.R
  05_results.qmd
  06_robustness.qmd

data/{raw,interim,meta}/    raw is gitignored
output/{results,meta}/      per-dataset caches + combined objects
figures/
```

---

## Extending the pipeline

- **Add a dataset (openESM):** it appears in `data/meta/`; Make picks it up. Nothing to code.
- **Add a dataset (other source):** add a raw loader + a harmonisation hook in `scripts/engine/harmonize.R`
  mapping it onto openESM conventions. Everything downstream is dataset-agnostic.
- **Add a model rung:** add one entry to `model_registry` in `scripts/engine/models.R` with the right
  `level` (`"person"` or `"dataset"`). CV, metrics, and meta-regression pick it up automatically.
- **Change the CV scheme:** rewrite `make_folds` in `scripts/engine/crossval.R`. The fold runner, models,
  and metrics are untouched — folds are just `list(train = <idx>, test = <idx>)`.

---

## The complexity ladder (current draft — see Open decisions)

| Rung | Model | `level` | Predictor for `Y[t,v]` |
|---|---|---|---|
| m0 | Person mean | person | training mean of `Y[,v]` |
| m1 | Deterministic trend | person | intercept + slope·`time[t]` |
| m2 | AR(1) | person | `φ_v · Ylag[t,v]` (own lag) |
| m3a | Pooled VAR (sample-average) | dataset | `Φ_{v,·} · Ylag[t,]`, common Φ |
| m3b | Person-specific VAR | person | `Φ_{v,·} · Ylag[t,]`, own Φ, no pooling |
| m4 | Random-effects mlVAR | dataset | `Φ_{v,·} · Ylag[t,]`, shrunken person-specific Φ |

---

## Open decisions (not yet settled — recorded so they aren't silently assumed)

- [ ] **Final model set.** Ladder above is a draft. Lag order fixed at 1? Contemporaneous
      (within-occasion) effects in the mlVAR rung, or lagged-only?
- [ ] **Primary CV scheme.** K-fold block CV (non-causal, defensible under the
      *predictability-of-process* estimand) as primary, with rolling-origin forward CV as
      robustness — or the reverse? Block size / K, or rolling horizon `h` and step.
- [ ] **Within-person standardization.** z-score each variable within person before
      fitting (makes R²/RMSE comparable) vs centering only. Confirm and document.
- [ ] **Trend handling.** m1 trend is a distinct rung (a separate model), not a
      preprocessing step applied to higher rungs. Confirm higher rungs do *not* pre-detrend.
- [ ] **Variable selection per dataset.** Which items enter the VAR; how many; missingness
      threshold for inclusion.
- [ ] **Unequal time intervals.** Policy for lagging across within-day gaps and the
      overnight boundary (no lag across nights?).
- [ ] **mlVAR estimation.** `mlVAR` package (two-step / lmer) vs Bayesian; default and any
      sensitivity check.
- [ ] **Meta-regression spec.** Individual- and study-level moderators; precision weighting
      by test-set size; how the two non-exchangeable data sources enter.

---

## Reproducibility

- Package management: `pacman` (matches the openESM-paper workflow).
- Orchestration: `make` (+ `make -jN`). `Makefile` documents targets.
- Tests live under `scripts/tests/`; run them directly with `Rscript scripts/tests/test_crossval_make_folds.R`.
- Raw data are gitignored (data-sharing agreements); `data/meta/` and processed
  derivatives are tracked where licensing permits.
