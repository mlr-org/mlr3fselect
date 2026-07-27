# mlr3fselect (development version)

* fix: `ArchiveAsyncFSelect` pushed results with the removed `rush::Rush$push_results()` method.
* fix: `ensemble_fselect()` dropped the `importance` column for subclasses of `FSelectorBatchRFE`. The column is now added whenever the feature selection result contains importance scores (#195).
* fix: The `mlr3fselect.svm_rfe` callback accepted support vector machines without a `type` or `kernel` setting, although only `type = "C-classification"` and `kernel = "linear"` are supported. The callback now also errors on multi-class tasks for which the importance scores are not defined (#173).
* fix: The asynchronous feature selection ignored the `always_included` column role. Columns with this role were excluded from the models instead of being added to every feature subset (#175).
* fix: The `mlr3fselect.one_se_rule` callback errored on archives with a single evaluation or with missing scores, and wrote the `n_features` column as a list column instead of an integer column (#174).
* fix: `extract_inner_fselect_results()` added the `iteration` and `fselect_instance` columns to the result of the inner `FSelectInstance` by reference, which created a circular reference between the instance and its own result (#172).
* fix: `fs("rfe")` and `fs("rfecv")` failed with an internal `data.table` error when `store_benchmark_result = FALSE` was set because the importance scores were read from the benchmark result of the archive (#169).
* fix: `fs("rfecv", recursive = FALSE)` failed with an internal `data.table` error because the importance scores of all resampling iterations were written to a single archive row (#168).
* fix: `fs("rfecv")` ignored the direction of the measure and selected the feature set size with the worst mean performance for minimizing measures such as `msr("classif.ce")` or `msr("regr.mse")`.
  Feature selection results obtained with `fs("rfecv")` and a minimizing measure are invalid and should be recomputed (#167).

# mlr3fselect 1.6.0

* refactor: Remove rush backward compatibility.
* docs: Add hEFS reference.
* compatibility: `fastVoteR` 0.0.3
* feat: Add `$rm_zero_features()` method in `EnsembleFSResult` to remove result rows where no features were selected.

# mlr3fselect 1.5.1

* compatibility: rush 1.0.0

# mlr3fselect 1.5.0

* fix: Add `always_included` column role to all registered tasks.
* perf: Add fast aggregation for `ResampleResult` and `BenchmarkResult` objects to speed up objective function evaluation.

# mlr3fselect 1.4.0

* feat: Introduce asynchronous optimization with the `FSelectorAsync` and `FSelectInstanceAsync*` classes.
* feat: Add `max_nfeatures` argument in the `pareto_front()` and `knee_points()` methods of an `EnsembleFSResult()`.
* feat: Classes are now printed with the `cli` package.

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
