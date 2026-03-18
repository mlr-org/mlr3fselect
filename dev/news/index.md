# Changelog

## mlr3fselect (development version)

## mlr3fselect 1.5.1

- compatibility: rush 1.0.0

## mlr3fselect 1.5.0

CRAN release: 2025-11-27

- fix: Add `always_included` column role to all registered tasks.
- perf: Add fast aggregation for `ResampleResult` and `BenchmarkResult`
  objects to speed up objective function evaluation.

## mlr3fselect 1.4.0

CRAN release: 2025-07-31

- feat: Introduce asynchronous optimization with the `FSelectorAsync`
  and `FSelectInstanceAsync*` classes.
- feat: Add `max_nfeatures` argument in the `pareto_front()` and
  `knee_points()` methods of an
  [`EnsembleFSResult()`](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md).
- feat: Classes are now printed with the `cli` package.

## mlr3fselect 1.3.0

CRAN release: 2025-01-16

- refactor: Use [fastVoteR](https://github.com/bblodfon/fastVoteR) for
  feature ranking in
  [`EnsembleFSResult()`](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md)
  objects
- feat: Add embedded ensemble feature selection
  [`embedded_ensemble_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/embedded_ensemble_fselect.md)
- refactor/perf:
  [`ensemble_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fselect.md)
  and
  [`EnsembleFSResult()`](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md)
- feat: Add `c.EnsembleFSResult(...)` and
  `EnsembleFSResult$combine(...)` methods

## mlr3fselect 1.2.1

CRAN release: 2024-11-07

- compatibility: mlr3 0.22.0

## mlr3fselect 1.2.0

CRAN release: 2024-10-25

- feat: Add internal tuning callback `mlr3fselect.internal_tuning`.
- fix: Register mlr3fselect in the `mlr_reflections$loaded_packages`
  field.

## mlr3fselect 1.1.1

CRAN release: 2024-10-15

- compatibility: bbotk 1.1.1

## mlr3fselect 1.1.0

CRAN release: 2024-09-09

- compatibility: mlr3 0.21.0
- fix: Delete intermediate `BenchmarkResult` in `ObjectiveFSelectBatch`
  after optimization.
- fix: Reloading mlr3fselect does not duplicate column roles anymore.
- perf: Remove `x_domain` column from archive.

## mlr3fselect 1.0.0

CRAN release: 2024-06-29

- feat: Add ensemble feature selection function
  [`ensemble_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fselect.md).
- BREAKING CHANGE: The `FSelector` class is `FSelectorBatch` now.
- BREAKING CHANGE: THe `FSelectInstanceSingleCrit` and
  `FSelectInstanceMultiCrit` classes are
  `FSelectInstanceBatchSingleCrit` and `FSelectInstanceBatchMultiCrit`
  now.
- BREAKING CHANGE: The `CallbackFSelect` class is `CallbackBatchFSelect`
  now.
- BREAKING CHANGE: The `ContextEval` class is `ContextBatchFSelect` now.

## mlr3fselect 0.12.0

CRAN release: 2024-03-09

- feat: Add number of features to `instance$result`.
- feat: Add `ties_method` options `"least_features"` and `"random"` to
  `ArchiveBatchFSelect$best()`.
- refactor: Optimize runtime of `ArchiveBatchFSelect$best()` method.
- feat: Add importance scores to result of `FSelectorRFE`.
- feat: Add number of features to `as.data.table.ArchiveBatchFSelect()`.
- feat: Features can be always included with the `always_include` column
  role.
- fix: Add `$phash()` method to `AutoFSelector`.
- fix: Include `FSelector` in hash of `AutoFSelector`.
- refactor: Change default batch size of `FSelectorBatchRandomSearch` to
  10.
- feat: Add `batch_size` parameter to `FSelectorBatchExhaustiveSearch`
  to reduce memory consumption.
- compatibility: Work with new paradox version 1.0.0

## mlr3fselect 0.11.0

CRAN release: 2023-03-02

- BREAKING CHANGE: The `method` parameter of
  [`fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect.md),
  [`fselect_nested()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect_nested.md)
  and
  [`auto_fselector()`](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md)
  is renamed to `fselector`. Only `FSelector` objects are accepted now.
  Arguments to the fselector cannot be passed with `...` anymore.
- BREAKING CHANGE: The `fselect` parameter of `FSelector` is moved to
  the first position to achieve consistency with the other functions.
- docs: Update resources sections.
- docs: Add list of default measures.

## mlr3fselect 0.10.0

CRAN release: 2023-02-21

- feat: Add callback `mlr3fselect.svm_rfe` to run recursive feature
  elimination on linear support vector machines.
- refactor: The importance scores in `FSelectorRFE` are now aggregated
  by rank instead of averaging them.
- feat: Add `FSelectorRFECV` optimizer to run recursive feature
  elimination with cross-validation.
- refactor: `FSelectorRFE` works without `store_models = TRUE` now.
- feat: The `as.data.table.ArchiveBatchFSelect()` function additionally
  returns a character vector of selected features for each row.
- refactor: Add `callbacks` argument to
  [`fsi()`](https://mlr3fselect.mlr-org.com/dev/reference/fsi.md)
  function.

## mlr3fselect 0.9.1

CRAN release: 2023-01-26

- refactor: Remove internal use of `mlr3pipelines`.
- fix: Feature selection with measures that require the importance or
  oob error works now.

## mlr3fselect 0.9.0

CRAN release: 2022-12-21

- fix: Add `genalg` to required packages of
  `FSelectorBatchGeneticSearch`.
- feat: Add new callback that backups the benchmark result to disk after
  each batch.
- feat: Create custom callbacks with the
  [`callback_batch_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/callback_batch_fselect.md)
  function.

## mlr3fselect 0.8.0

CRAN release: 2022-11-16

- refactor: `FSelectorRFE` throws an error if the learner does not
  support the `$importance()` method.
- refactor: The `AutoFSelector` stores the instance and benchmark result
  if `store_models = TRUE`.
- refactor: The `AutoFSelector` stores the instance if
  `store_benchmark_result = TRUE`.
- feat: Add missing parameters from `AutoFSelector` to `auto_fselect()`.
- feat: Add
  [`fsi()`](https://mlr3fselect.mlr-org.com/dev/reference/fsi.md)
  function to create a `FSelectInstanceBatchSingleCrit` or
  `FSelectInstanceBatchMultiCrit`.
- refactor: Remove `unnest` option from
  `as.data.table.ArchiveBatchFSelect()` function.

## mlr3fselect 0.7.2

CRAN release: 2022-08-25

- docs: Re-generate rd files with valid html.

## mlr3fselect 0.7.1

CRAN release: 2022-05-03

- feat: `FSelector` objects have the field `$id` now.

## mlr3fselect 0.7.0

CRAN release: 2022-04-08

- feat: Allow to pass `FSelector` objects as `method` in
  [`fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect.md)
  and
  [`auto_fselector()`](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md).
- feat: Added `$label` to `FSelector`s.
- docs: New examples with
  [`fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect.md)
  function.
- feat: `$help()` method which opens manual page of a `FSelector`.
- feat: Added a `as.data.table.DictionaryFSelector` function.
- feat: Added `min_features` parameter to `FSelectorBatchSequential`.

## mlr3fselect 0.6.1

CRAN release: 2022-01-20

- Add `store_models` flag to
  [`fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect.md).
- Remove `store_x_domain` flag.

## mlr3fselect 0.6.0

CRAN release: 2021-09-13

- Adds `AutoFSelector$base_learner()` method to extract the base learner
  from nested learner objects.
- Adds
  [`fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect.md),
  [`auto_fselector()`](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md)
  and
  [`fselect_nested()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect_nested.md)
  sugar functions.
- Adds
  [`extract_inner_fselect_results()`](https://mlr3fselect.mlr-org.com/dev/reference/extract_inner_fselect_results.md)
  and
  [`extract_inner_fselect_archives()`](https://mlr3fselect.mlr-org.com/dev/reference/extract_inner_fselect_archives.md)
  helper function to extract inner feature selection results and
  archives.

## mlr3fselect 0.5.1

CRAN release: 2021-03-09

- Remove `x_domain` column from archive.

## mlr3fselect 0.5.0

CRAN release: 2021-01-24

- `FSelectorRFE` stores importance values of each evaluated feature set
  in archive.
- `ArchiveBatchFSelect$data` is a public field now.

## mlr3fselect 0.4.1

CRAN release: 2020-10-30

- Fix bug in `AutoFSelector$predict()`

## mlr3fselect 0.4.0

CRAN release: 2020-10-22

- Compact in-memory representation of R6 objects to save space when
  saving mlr3 objects via saveRDS(), serialize() etc.
- `FSelectorRFE` supports fraction of features to retain in each
  iteration (`feature_fraction`), number of features to remove in each
  iteration (`feature_number`) and vector of number of features to
  retain in each iteration (`subset_sizes`).
- `AutoFSelect` is renamed to `AutoFSelector`.
- To retrieve the inner feature selection results in nested resampling,
  `as.data.table(rr)$learner[[1]]$fselect_result` must be used now.
- Option to control `store_benchmark_result`, `store_models` and
  `check_values` in `AutoFSelector`. `store_fselect_instance` must be
  set as a parameter during initialization.
- Adds `FSelectorBatchGeneticSearch`.
- Fixes `check_values` flag in `FSelectInstanceBatchSingleCrit` and
  `FSelectInstanceBatchMultiCrit`.
- Removed dependency on orphaned package `bibtex`.
- `PipeOpSelect` is internally used for task subsetting.

## mlr3fselect 0.3.0

CRAN release: 2020-09-22

- `Archive` is `ArchiveBatchFSelect` now which stores the benchmark
  result in `$benchmark_result`. This change removed the resample
  results from the archive but they can be still accessed via the
  benchmark result.

## mlr3fselect 0.2.1

CRAN release: 2020-09-10

- Warning message if external package for feature selection is not
  installed.

## mlr3fselect 0.2.0

CRAN release: 2020-08-23

- Initial CRAN release.
