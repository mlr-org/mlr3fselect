# mlr3fselect 0.5.1.9000

- Same as previous version.


# mlr3fselect 0.5.1

- Remove `x_domain` column from archive.

# mlr3fselect 0.5.0

- `FSelectorRFE` stores importance values of each evaluated feature set in 
  archive.
- `ArchiveFSelect$data` is a public field now.

# mlr3fselect 0.4.1

- Fix bug in `AutoFSelector$predict()`

# mlr3fselect 0.4.0

- Compact in-memory representation of R6 objects to save space when saving mlr3
  objects via saveRDS(), serialize() etc.
- `FSelectorRFE` supports fraction of features to retain in each iteration
  (`feature_fraction`), number of features to remove in each iteration
  (`feature_number`) and vector of number of features to retain in each
  iteration (`subset_sizes`).
- `AutoFSelect` is renamed to `AutoFSelector`.
- To retrieve the inner feature selection results in nested resampling,
  `as.data.table(rr)$learner[[1]]$fselect_result` must be used now.
- Option to control `store_benchmark_result`, `store_models` and `check_values`
  in `AutoFSelector`. `store_fselect_instance` must be set as a parameter during
  initialization.
- Adds `FSelectorGeneticSearch`.
- Fixes `check_values` flag in `FSelectInstanceSingleCrit` and
  `FSelectInstanceMultiCrit`.
- Removed dependency on orphaned package `bibtex`.
- `PipeOpSelect` is internally used for task subsetting.

# mlr3fselect 0.3.0

- `Archive` is `ArchiveFSelect` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
  but they can be still accessed via the benchmark result.

# mlr3fselect 0.2.1

- Warning message if external package for feature selection is not installed.

# mlr3fselect 0.2.0

- Initial CRAN release.
