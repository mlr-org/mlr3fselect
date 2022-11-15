# mlr3fselect 0.7.2.9000

* refactor: `FSelectorRFE` throws an error if the learner does not support the `$importance()` method.
* refactor: The `AutoFSelector` stores the instance and benchmark result if `store_models = TRUE`.
* refactor: The `AutoFSelector` stores the instance if `store_benchmark_result = TRUE`.
* feat: Add missing parameters from `AutoFSelector` to `auto_fselect()`.
* feat: Add `fsi()` function to create a `FSelectInstanceSingleCrit` or `FSelectInstanceMultiCrit`.
* refactor: Remove `unnest` option from `as.data.table.ArchiveFSelect()` function.

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
* feat: Added `min_features` parameter to `FSelectorSequential`.

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
* `ArchiveFSelect$data` is a public field now.

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
* Adds `FSelectorGeneticSearch`.
* Fixes `check_values` flag in `FSelectInstanceSingleCrit` and
  `FSelectInstanceMultiCrit`.
* Removed dependency on orphaned package `bibtex`.
* `PipeOpSelect` is internally used for task subsetting.

# mlr3fselect 0.3.0

* `Archive` is `ArchiveFSelect` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
  but they can be still accessed via the benchmark result.

# mlr3fselect 0.2.1

* Warning message if external package for feature selection is not installed.

# mlr3fselect 0.2.0

* Initial CRAN release.
