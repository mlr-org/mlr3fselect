# Frozen Rush Data Storage

Freezes the Redis data base of an
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/ArchiveAsyncFSelect.md)
to a
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
No further points can be added to the archive but the data can be
accessed and analyzed. Useful when the Redis data base is not
permanently available. Use the callback
[mlr3fselect.async_freeze_archive](https://mlr3fselect.mlr-org.com/reference/mlr3fselect.async_freeze_archive.md)
to freeze the archive after the optimization has finished.

## S3 Methods

- `as.data.table(archive)`  
  ArchiveAsyncFSelectFrozen -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a tabular view of all performed function calls of the
  Objective.

## Super classes

[`bbotk::Archive`](https://bbotk.mlr-org.com/reference/Archive.html) -\>
[`bbotk::ArchiveAsync`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
-\>
[`bbotk::ArchiveAsyncFrozen`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html)
-\> `ArchiveAsyncFSelectFrozen`

## Active bindings

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  Benchmark result.

## Methods

### Public methods

- [`ArchiveAsyncFSelectFrozen$new()`](#method-ArchiveAsyncFSelectFrozen-new)

- [`ArchiveAsyncFSelectFrozen$learner()`](#method-ArchiveAsyncFSelectFrozen-learner)

- [`ArchiveAsyncFSelectFrozen$learners()`](#method-ArchiveAsyncFSelectFrozen-learners)

- [`ArchiveAsyncFSelectFrozen$predictions()`](#method-ArchiveAsyncFSelectFrozen-predictions)

- [`ArchiveAsyncFSelectFrozen$resample_result()`](#method-ArchiveAsyncFSelectFrozen-resample_result)

- [`ArchiveAsyncFSelectFrozen$print()`](#method-ArchiveAsyncFSelectFrozen-print)

- [`ArchiveAsyncFSelectFrozen$clone()`](#method-ArchiveAsyncFSelectFrozen-clone)

Inherited methods

- [`bbotk::Archive$format()`](https://bbotk.mlr-org.com/reference/Archive.html#method-format)
- [`bbotk::Archive$help()`](https://bbotk.mlr-org.com/reference/Archive.html#method-help)
- [`bbotk::ArchiveAsync$best()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-best)
- [`bbotk::ArchiveAsync$nds_selection()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-nds_selection)
- [`bbotk::ArchiveAsyncFrozen$clear()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-clear)
- [`bbotk::ArchiveAsyncFrozen$data_with_state()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-data_with_state)
- [`bbotk::ArchiveAsyncFrozen$pop_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-pop_point)
- [`bbotk::ArchiveAsyncFrozen$push_failed_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_failed_point)
- [`bbotk::ArchiveAsyncFrozen$push_points()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_points)
- [`bbotk::ArchiveAsyncFrozen$push_result()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_result)
- [`bbotk::ArchiveAsyncFrozen$push_running_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_running_point)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ArchiveAsyncFSelectFrozen$new(archive)

#### Arguments

- `archive`:

  ([ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/ArchiveAsyncFSelect.md))  
  The archive to freeze.

------------------------------------------------------------------------

### Method `learner()`

Retrieve
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) of the
i-th evaluation, by position or by unique hash `uhash`. `i` and `uhash`
are mutually exclusive. Learner does not contain a model. Use
`$learners()` to get learners with models.

#### Usage

    ArchiveAsyncFSelectFrozen$learner(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelectFrozen$learners(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelectFrozen$predictions(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelectFrozen$resample_result(i = NULL, uhash = NULL)

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

    ArchiveAsyncFSelectFrozen$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchiveAsyncFSelectFrozen$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
