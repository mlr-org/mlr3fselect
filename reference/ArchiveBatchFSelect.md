# Class for Logging Evaluated Feature Sets

The ArchiveBatchFSelect stores all evaluated feature sets and
performance scores.

## Details

The ArchiveBatchFSelect is a container around a
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
Each row corresponds to a single evaluation of a feature set. See the
section on Data Structure for more information. The archive stores
additionally a
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
(`$benchmark_result`) that records the resampling experiments. Each
experiment corresponds to a single evaluation of a feature set. The
table (`$data`) and the benchmark result (`$benchmark_result`) are
linked by the `uhash` column. If the archive is passed to
`as.data.table()`, both are joined automatically.

## Data structure

The table (`$data`) has the following columns:

- One column for each feature of the task (`$search_space`).

- One column for each performance measure (`$codomain`).

- `runtime_learners` (`numeric(1)`)  
  Sum of training and predict times logged in learners per
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  / evaluation. This does not include potential overhead time.

- `timestamp` (`POSIXct`)  
  Time stamp when the evaluation was logged into the archive.

- `batch_nr` (`integer(1)`)  
  Feature sets are evaluated in batches. Each batch has a unique batch
  number.

- `uhash` (`character(1)`)  
  Connects each feature set to the resampling experiment stored in the
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

## Analysis

For analyzing the feature selection results, it is recommended to pass
the archive to `as.data.table()`. The returned data table is joined with
the benchmark result which adds the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each feature set.

The archive provides various getters (e.g. `$learners()`) to ease the
access. All getters extract by position (`i`) or unique hash (`uhash`).
For a complete list of all getters see the methods section.

The benchmark result (`$benchmark_result`) allows to score the feature
sets again on a different measure. Alternatively, measures can be
supplied to `as.data.table()`.

## S3 Methods

- `as.data.table.ArchiveBatchFSelect(x, exclude_columns = "uhash", measures = NULL)`  
  Returns a tabular view of all evaluated feature sets.  
  ArchiveBatchFSelect -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  

  - `x` (ArchiveBatchFSelect)

  - `exclude_columns`
    ([`character()`](https://rdrr.io/r/base/character.html))  
    Exclude columns from table. Set to `NULL` if no column should be
    excluded.

  - `measures` (list of
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
    Score feature sets on additional measures.

## Super classes

[`bbotk::Archive`](https://bbotk.mlr-org.com/reference/Archive.html) -\>
[`bbotk::ArchiveBatch`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
-\> `ArchiveBatchFSelect`

## Public fields

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  Benchmark result.

## Active bindings

- `ties_method`:

  (`character(1)`)  
  Method to handle ties.

## Methods

### Public methods

- [`ArchiveBatchFSelect$new()`](#method-ArchiveBatchFSelect-new)

- [`ArchiveBatchFSelect$add_evals()`](#method-ArchiveBatchFSelect-add_evals)

- [`ArchiveBatchFSelect$learner()`](#method-ArchiveBatchFSelect-learner)

- [`ArchiveBatchFSelect$learners()`](#method-ArchiveBatchFSelect-learners)

- [`ArchiveBatchFSelect$predictions()`](#method-ArchiveBatchFSelect-predictions)

- [`ArchiveBatchFSelect$resample_result()`](#method-ArchiveBatchFSelect-resample_result)

- [`ArchiveBatchFSelect$print()`](#method-ArchiveBatchFSelect-print)

- [`ArchiveBatchFSelect$best()`](#method-ArchiveBatchFSelect-best)

- [`ArchiveBatchFSelect$clone()`](#method-ArchiveBatchFSelect-clone)

Inherited methods

- [`bbotk::Archive$format()`](https://bbotk.mlr-org.com/reference/Archive.html#method-format)
- [`bbotk::Archive$help()`](https://bbotk.mlr-org.com/reference/Archive.html#method-help)
- [`bbotk::ArchiveBatch$clear()`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html#method-clear)
- [`bbotk::ArchiveBatch$nds_selection()`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html#method-nds_selection)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ArchiveBatchFSelect$new(
      search_space,
      codomain,
      check_values = TRUE,
      ties_method = "least_features"
    )

#### Arguments

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space. Internally created from provided
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) by
  instance.

- `codomain`:

  ([bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html))  
  Specifies codomain of objective function i.e. a set of performance
  measures. Internally created from provided
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)s by
  instance.

- `check_values`:

  (`logical(1)`)  
  If `TRUE` (default), hyperparameter configurations are check for
  validity.

- `ties_method`:

  (`character(1)`)  
  The method to break ties when selecting sets while optimizing and when
  selecting the best set. Can be `"least_features"` or `"random"`. The
  option `"least_features"` (default) selects the feature set with the
  least features. If there are multiple best feature sets with the same
  number of features, one is selected randomly. The `random` method
  returns a random feature set from the best feature sets. Ignored if
  multiple measures are used.

------------------------------------------------------------------------

### Method `add_evals()`

Adds function evaluations to the archive table.

#### Usage

    ArchiveBatchFSelect$add_evals(xdt, xss_trafoed = NULL, ydt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  x values as `data.table`. Each row is one point. Contains the value in
  the *search space* of the
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md)
  object. Can contain additional columns for extra information.

- `xss_trafoed`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Ignored in feature selection.

- `ydt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Optimal outcome.

------------------------------------------------------------------------

### Method `learner()`

Retrieve
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) of the
i-th evaluation, by position or by unique hash `uhash`. `i` and `uhash`
are mutually exclusive. Learner does not contain a model. Use
`$learners()` to get learners with models.

#### Usage

    ArchiveBatchFSelect$learner(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `learners()`

Retrieve list of trained
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) objects
of the i-th evaluation, by position or by unique hash `uhash`. `i` and
`uhash` are mutually exclusive.

#### Usage

    ArchiveBatchFSelect$learners(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `predictions()`

Retrieve list of
[mlr3::Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)
objects of the i-th evaluation, by position or by unique hash `uhash`.
`i` and `uhash` are mutually exclusive.

#### Usage

    ArchiveBatchFSelect$predictions(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `resample_result()`

Retrieve
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
of the i-th evaluation, by position or by unique hash `uhash`. `i` and
`uhash` are mutually exclusive.

#### Usage

    ArchiveBatchFSelect$resample_result(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    ArchiveBatchFSelect$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `best()`

Returns the best scoring feature sets.

#### Usage

    ArchiveBatchFSelect$best(batch = NULL, ties_method = NULL)

#### Arguments

- `batch`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The batch number(s) to limit the best results to. Default is all
  batches.

- `ties_method`:

  (`character(1)`)  
  Method to handle ties. If `NULL` (default), the global ties method set
  during initialization is used. The default global ties method is
  `least_features` which selects the feature set with the least
  features. If there are multiple best feature sets with the same number
  of features, one is selected randomly. The `random` method returns a
  random feature set from the best feature sets.

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchiveBatchFSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
