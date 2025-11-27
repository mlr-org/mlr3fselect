# Asynchronous Feature Selection Context

A
[CallbackAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackAsyncFSelect.md)
accesses and modifies data during the optimization via the
`ContextAsyncFSelect`. See the section on active bindings for a list of
modifiable objects. See
[`callback_async_fselect()`](https://mlr3fselect.mlr-org.com/reference/callback_async_fselect.md)
for a list of stages that access `ContextAsyncFSelect`.

## Details

Changes to `$instance` and `$optimizer` in the stages executed on the
workers are not reflected in the main process.

## Super classes

[`mlr3misc::Context`](https://mlr3misc.mlr-org.com/reference/Context.html)
-\>
[`bbotk::ContextAsync`](https://bbotk.mlr-org.com/reference/ContextAsync.html)
-\> `ContextAsyncFSelect`

## Public fields

- `auto_fselector`:

  ([AutoFSelector](https://mlr3fselect.mlr-org.com/reference/AutoFSelector.md))  
  The
  [AutoFSelector](https://mlr3fselect.mlr-org.com/reference/AutoFSelector.md)
  instance.

## Active bindings

- `xs_objective`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  The feature subset currently evaluated.

- `resample_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  The resample result of the feature subset currently evaluated.

- `aggregated_performance`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Aggregated performance scores and training time of the evaluated
  feature subset. This list is passed to the archive. A callback can add
  additional elements which are also written to the archive.

- `result_feature_set`:

  (character())  
  The feature set passed to `instance$assign_result()`.

## Methods

### Public methods

- [`ContextAsyncFSelect$clone()`](#method-ContextAsyncFSelect-clone)

Inherited methods

- [`mlr3misc::Context$format()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-format)
- [`mlr3misc::Context$print()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-print)
- [`bbotk::ContextAsync$initialize()`](https://bbotk.mlr-org.com/reference/ContextAsync.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContextAsyncFSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
