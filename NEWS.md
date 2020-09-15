# mlr3fselector 0.3.0.9000

* Compact in-memory representation of R6 objects to save space when saving mlr3
  objects via saveRDS(), serialize() etc.
* `FSelectorRFE` supports fraction of features to retain in each iteration
  (`feature_fraction`), number of features to remove in each iteration
  (`feature_number`) and vector of number of features to retain in each 
  iteration (`subset_sizes`).

# mlr3fselect 0.3.0

* `Archive` is `ArchiveFSelect` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
   but they can be still accessed via the benchmark result.

# mlr3fselect 0.2.1

* Warning message if external package for feature selection is not installed.

# mlr3fselect 0.2.0

* Initial CRAN release.
