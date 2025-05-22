# mlr3fselect (development version)

* feat: Add `max_nfeatures` argument in the `pareto_front()` and `knee_points()` methods of an `EnsembleFSResult()`

# mlr3fselect 1.3.0

* refactor: Use [fastVoteR](https://github.com/bblodfon/fastVoteR) for feature ranking in `EnsembleFSResult()` objects
* feat: Add embedded ensemble feature selection `embedded_ensemble_fselect()`
* refactor/perf: `ensemble_fselect()` and `EnsembleFSResult()`
* feat: Add `c.EnsembleFSResult(...)` and `EnsembleFSResult$combine(...)` methods

# mlr3fselect 1.2.1

* compatibility: mlr3 0.22.0

# mlr3fselect 1.2.0

* feat: Add internal tuning callback `mlr3fselect.internal_tuning`.
* fix: Register mlr3fselect in the `mlr_reflections$loaded_packages` field.

# mlr3fselect 1.1.1

* compatibility: bbotk 1.1.1

# mlr3fselect 1.1.0

* compatibility: mlr3 0.21.0
* fix: Delete intermediate `BenchmarkResult` in `ObjectiveFSelectBatch` after optimization.
* fix: Reloading mlr3fselect does not duplicate column roles anymore.
* perf: Remove `x_domain` column from archive.

# mlr3fselect 1.0.0

* feat: Add ensemble feature selection function `ensemble_fselect()`.
* BREAKING CHANGE: The `FSelector` class is `FSelectorBatch` now.
* BREAKING CHANGE: THe `FSelectInstanceSingleCrit` and `FSelectInstanceMultiCrit` classes are `FSelectInstanceBatchSingleCrit` and `FSelectInstanceBatchMultiCrit` now.
* BREAKING CHANGE: The `CallbackFSelect` class is `CallbackBatchFSelect` now.
* BREAKING CHANGE: The `ContextEval` class is `ContextBatchFSelect` now.

# mlr3fselect 0.12.0

* feat: Add number of features to `instance$result`.
* feat: Add `ties_method` options `"least_features"` and `"random"` to `ArchiveBatchFSelect$best()`.
* refactor: Optimize runtime of `ArchiveBatchFSelect$best()` method.
* feat: Add importance scores to result of `FSelectorRFE`.
* feat: Add number of features to `as.data.table.ArchiveBatchFSelect()`.
* feat: Features can be always included with the `always_include` column role.
* fix: Add `$phash()` method to `AutoFSelector`.
* fix: Include `FSelector` in hash of  `AutoFSelector`.
* refactor: Change default batch size of `FSelectorBatchRandomSearch` to 10.
* feat: Add `batch_size` parameter to `FSelectorBatchExhaustiveSearch` to reduce memory consumption.
* compatibility: Work with new paradox version 1.0.0

# mlr3fselect 0.11.0

* BREAKING CHANGE: The `method` parameter of `fselect()`, `fselect_nested()` and `auto_fselector()` is renamed to `fselector`.
  Only `FSelector` objects are accepted now.
  Arguments to the fselector cannot be passed with `...` anymore.
* BREAKING CHANGE: The `fselect` parameter of `FSelector` is moved to the first position to achieve consistency with the other functions.
* docs: Update resources sections.
* docs: Add list of default measures.

# mlr3fselect 0.10.0

* feat: Add callback `mlr3fselect.svm_rfe` to run recursive feature elimination on linear support vector machines.
* refactor: The importance scores in `FSelectorRFE` are now aggregated by rank instead of averaging them.
* feat: Add `FSelectorRFECV` optimizer to run recursive feature elimination with cross-validation.
* refactor: `FSelectorRFE` works without `store_models = TRUE` now.
* feat: The `as.data.table.ArchiveBatchFSelect()` function additionally returns a character vector of selected features for each row.
* refactor: Add `callbacks` argument to `fsi()` function.

# mlr3fselect 0.9.1

* refactor: Remove internal use of `mlr3pipelines`.
* fix: Feature selection with measures that require the importance or oob error works now.

# mlr3fselect 0.9.0

* fix: Add `genalg` to required packages of `FSelectorBatchGeneticSearch`.
* feat: Add new callback that backups the benchmark result to disk after each batch.
* feat: Create custom callbacks with the `callback_batch_fselect()` function.

# mlr3fselect 0.8.0

* refactor: `FSelectorRFE` throws an error if the learner does not support the `$importance()` method.
* refactor: The `AutoFSelector` stores the instance and benchmark result if `store_models = TRUE`.
* refactor: The `AutoFSelector` stores the instance if `store_benchmark_result = TRUE`.
* feat: Add missing parameters from `AutoFSelector` to `auto_fselect()`.
* feat: Add `fsi()` function to create a `FSelectInstanceBatchSingleCrit` or `FSelectInstanceBatchMultiCrit`.
* refactor: Remove `unnest` option from `as.data.table.ArchiveBatchFSelect()` function.

# mlr3fselect 0.7.2

* docs: Re-generate rd files with valid html.

# mlr3fselect 0.7.1

* feat: `FSelector` objects have the field `$id` now.

# mlr3fselect 0.7.0

* feat: Allow to pass `FSelector` objects as `method` in `fselect()` and `auto_fselector()`.
* feat: Added `$label` to `FSelector`s.
* docs: New examples with `fselect()` function.
* feat: `$help()` method which opens manual page of a `FSelector`.
* feat: Added a `as.data.table.DictionaryFSelector` function.
* feat: Added `min_features` parameter to `FSelectorBatchSequential`.

# mlr3fselect 0.6.1

* Add `store_models` flag to `fselect()`.
* Remove `store_x_domain` flag.

# mlr3fselect 0.6.0

* Adds `AutoFSelector$base_learner()` method to extract the base learner from
  nested learner objects.
* Adds `fselect()`, `auto_fselector()` and `fselect_nested()` sugar functions.
* Adds `extract_inner_fselect_results()` and `extract_inner_fselect_archives()`
  helper function to extract inner feature selection results and archives.

# mlr3fselect 0.5.1

* Remove `x_domain` column from archive.

# mlr3fselect 0.5.0

* `FSelectorRFE` stores importance values of each evaluated feature set in
  archive.
* `ArchiveBatchFSelect$data` is a public field now.

# mlr3fselect 0.4.1

* Fix bug in `AutoFSelector$predict()`

# mlr3fselect 0.4.0

* Compact in-memory representation of R6 objects to save space when saving mlr3
  objects via saveRDS(), serialize() etc.
* `FSelectorRFE` supports fraction of features to retain in each iteration
  (`feature_fraction`), number of features to remove in each iteration
  (`feature_number`) and vector of number of features to retain in each
  iteration (`subset_sizes`).
* `AutoFSelect` is renamed to `AutoFSelector`.
* To retrieve the inner feature selection results in nested resampling,
  `as.data.table(rr)$learner[[1]]$fselect_result` must be used now.
* Option to control `store_benchmark_result`, `store_models` and `check_values`
  in `AutoFSelector`. `store_fselect_instance` must be set as a parameter during
  initialization.
* Adds `FSelectorBatchGeneticSearch`.
* Fixes `check_values` flag in `FSelectInstanceBatchSingleCrit` and
  `FSelectInstanceBatchMultiCrit`.
* Removed dependency on orphaned package `bibtex`.
* `PipeOpSelect` is internally used for task subsetting.

# mlr3fselect 0.3.0

* `Archive` is `ArchiveBatchFSelect` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
  but they can be still accessed via the benchmark result.

# mlr3fselect 0.2.1

* Warning message if external package for feature selection is not installed.

# mlr3fselect 0.2.0

* Initial CRAN release.
