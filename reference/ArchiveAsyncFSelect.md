# Rush Data Storage

The `ArchiveAsyncFSelect` stores all evaluated feature subsets and
performance scores in a
[rush::Rush](https://rush.mlr-org.com/reference/Rush.html) database.

## Details

The ArchiveAsyncFSelect is a connector to a
[rush::Rush](https://rush.mlr-org.com/reference/Rush.html) database.

## Data Structure

The table (`$data`) has the following columns:

- One column for each feature of the search space (`$search_space`).

- One column for each performance measure (`$codomain`).

- `runtime_learners` (`numeric(1)`)  
  Sum of training and predict times logged in learners per
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  / evaluation. This does not include potential overhead time.

- `timestamp` (`POSIXct`)  
  Time stamp when the evaluation was logged into the archive.

## Analysis

For analyzing the feature selection results, it is recommended to pass
the ArchiveAsyncFSelect to `as.data.table()`. The returned data table
contains the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each feature subset evaluation.

## S3 Methods

- `as.data.table.ArchiveFSelect(x, unnest = "x_domain", exclude_columns = "uhash", measures = NULL)`  
  Returns a tabular view of all evaluated feature subsets.  
  ArchiveAsyncFSelect -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  

  - `x` (ArchiveAsyncFSelect)

  - `unnest` ([`character()`](https://rdrr.io/r/base/character.html))  
    Transforms list columns to separate columns. Set to `NULL` if no
    column should be unnested.

  - `exclude_columns`
    ([`character()`](https://rdrr.io/r/base/character.html))  
    Exclude columns from table. Set to `NULL` if no column should be
    excluded.

  - `measures` (List of
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
    Score feature subsets on additional measures.

## Super classes

[`bbotk::Archive`](https://bbotk.mlr-org.com/reference/Archive.html) -\>
[`bbotk::ArchiveAsync`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
-\> `ArchiveAsyncFSelect`

## Active bindings

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  Benchmark result.

- `ties_method`:

  (`character(1)`)  
  Method to handle ties in the archive. One of `"least_features"`
  (default) or `"random"`.

## Methods

### Public methods

- [`ArchiveAsyncFSelect$new()`](#method-ArchiveAsyncFSelect-new)

- [`ArchiveAsyncFSelect$learner()`](#method-ArchiveAsyncFSelect-learner)

- [`ArchiveAsyncFSelect$learners()`](#method-ArchiveAsyncFSelect-learners)

- [`ArchiveAsyncFSelect$predictions()`](#method-ArchiveAsyncFSelect-predictions)

- [`ArchiveAsyncFSelect$resample_result()`](#method-ArchiveAsyncFSelect-resample_result)

- [`ArchiveAsyncFSelect$print()`](#method-ArchiveAsyncFSelect-print)

- [`ArchiveAsyncFSelect$best()`](#method-ArchiveAsyncFSelect-best)

- [`ArchiveAsyncFSelect$push_result()`](#method-ArchiveAsyncFSelect-push_result)

- [`ArchiveAsyncFSelect$clone()`](#method-ArchiveAsyncFSelect-clone)

Inherited methods

- [`bbotk::Archive$format()`](https://bbotk.mlr-org.com/reference/Archive.html#method-format)
- [`bbotk::Archive$help()`](https://bbotk.mlr-org.com/reference/Archive.html#method-help)
- [`bbotk::ArchiveAsync$clear()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-clear)
- [`bbotk::ArchiveAsync$data_with_state()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-data_with_state)
- [`bbotk::ArchiveAsync$nds_selection()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-nds_selection)
- [`bbotk::ArchiveAsync$pop_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-pop_point)
- [`bbotk::ArchiveAsync$push_failed_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_failed_point)
- [`bbotk::ArchiveAsync$push_points()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_points)
- [`bbotk::ArchiveAsync$push_running_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_running_point)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ArchiveAsyncFSelect$new(
      search_space,
      codomain,
      rush,
      ties_method = "least_features"
    )

#### Arguments

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space. Internally created from provided
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) by
  instance.

- `codomain`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Specifies codomain of function. Most importantly the tags of each
  output "Parameter" define whether it should be minimized or maximized.
  The default is to minimize each component.

- `rush`:

  (`Rush`)  
  If a rush instance is supplied, the optimization runs without batches.

- `ties_method`:

  (`character(1)`)  
  The method to break ties when selecting sets while optimizing and when
  selecting the best set. Can be `"least_features"` or `"random"`. The
  option `"least_features"` (default) selects the feature set with the
  least features. If there are multiple best feature sets with the same
  number of features, one is selected randomly. The `random` method
  returns a random feature set from the best feature sets. Ignored if
  multiple measures are used.

- `check_values`:

  (`logical(1)`)  
  If `TRUE` (default), feature subsets are check for validity.

------------------------------------------------------------------------

### Method `learner()`

Retrieve
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) of the
i-th evaluation, by position or by unique hash `uhash`. `i` and `uhash`
are mutually exclusive. Learner does not contain a model. Use
`$learners()` to get learners with models.

#### Usage

    ArchiveAsyncFSelect$learner(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelect$learners(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelect$predictions(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelect$resample_result(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelect$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `best()`

Returns the best scoring feature set(s). For single-crit optimization,
the solution that minimizes / maximizes the objective function. For
multi-crit optimization, the Pareto set / front.

#### Usage

    ArchiveAsyncFSelect$best(n_select = 1, ties_method = "least_features")

#### Arguments

- `n_select`:

  (`integer(1L)`)  
  Amount of points to select. Ignored for multi-crit optimization.

- `ties_method`:

  (`character(1L)`)  
  Method to break ties when multiple points have the same score. Either
  `"least_features"` (default) or `"random"`. Ignored for multi-crit
  optimization. If `n_select > 1L`, the tie method is ignored and the
  first point is returned.

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `push_result()`

Push result to the archive.

#### Usage

    ArchiveAsyncFSelect$push_result(key, ys, x_domain, extra = NULL)

#### Arguments

- `key`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Key of the point.

- `ys`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Named list of results.

- `x_domain`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Is ignored for feature selection.

- `extra`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Named list of additional information.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchiveAsyncFSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
