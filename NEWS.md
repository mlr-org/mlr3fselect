# mlr3fselector 0.3.0.9000

* Compact in-memory representation of R6 objects to save space when saving mlr3
  objects via saveRDS(), serialize() etc.
* `AutoFSelect` is renamed to `AutoFSelector`.
* To retrieve the inner feature selection results in nested resampling,
  as.data.table(rr)$learner[[1]]$fselect_result must be used now.
* Option to control `store_benchmark_result`, `store_models` and `check_values`
  in `AutoFSelector`. `store_fselect_instance` must be set as a parameter during 
  initialization.

# mlr3fselect 0.3.0

* `Archive` is `ArchiveFSelect` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
   but they can be still accessed via the benchmark result.

# mlr3fselect 0.2.1

* Warning message if external package for feature selection is not installed.

# mlr3fselect 0.2.0

* Initial CRAN release.
