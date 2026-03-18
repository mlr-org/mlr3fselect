# Evaluation Context

The ContextBatchFSelect allows
[CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackBatchFSelect.md)s
to access and modify data while a batch of feature sets is evaluated.
See the section on active bindings for a list of modifiable objects. See
[`callback_batch_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/callback_batch_fselect.md)
for a list of stages that access ContextBatchFSelect.

## Details

This context is re-created each time a new batch of feature sets is
evaluated. Changes to `$objective_fselect`, `$design`
`$benchmark_result` are discarded after the function is finished.
Modification on the data table in `$aggregated_performance` are written
to the archive. Any number of columns can be added.

## Super classes

[`mlr3misc::Context`](https://mlr3misc.mlr-org.com/reference/Context.html)
-\>
[`bbotk::ContextBatch`](https://bbotk.mlr-org.com/reference/ContextBatch.html)
-\> `ContextBatchFSelect`

## Public fields

- `auto_fselector`:

  ([AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md))  
  The
  [AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
  instance.

## Active bindings

- `xss`:

  (list())  
  The feature sets of the latest batch.

- `design`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  The benchmark design of the latest batch.

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  The benchmark result of the latest batch.

- `aggregated_performance`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Aggregated performance scores and training time of the latest batch.
  This data table is passed to the archive. A callback can add
  additional columns which are also written to the archive.

## Methods

### Public methods

- [`ContextBatchFSelect$clone()`](#method-ContextBatchFSelect-clone)

Inherited methods

- [`mlr3misc::Context$format()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-format)
- [`mlr3misc::Context$print()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-print)
- [`bbotk::ContextBatch$initialize()`](https://bbotk.mlr-org.com/reference/ContextBatch.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContextBatchFSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
