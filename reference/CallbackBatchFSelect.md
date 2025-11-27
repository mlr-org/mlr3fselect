# Create Feature Selection Callback

Specialized
[bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html)
for feature selection. Callbacks allow customizing the behavior of
processes in mlr3fselect. The
[`callback_batch_fselect()`](https://mlr3fselect.mlr-org.com/reference/callback_batch_fselect.md)
function creates a CallbackBatchFSelect. Predefined callbacks are stored
in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html). For more
information on callbacks see
[`callback_batch_fselect()`](https://mlr3fselect.mlr-org.com/reference/callback_batch_fselect.md).

## Super classes

[`mlr3misc::Callback`](https://mlr3misc.mlr-org.com/reference/Callback.html)
-\>
[`bbotk::CallbackBatch`](https://bbotk.mlr-org.com/reference/CallbackBatch.html)
-\> `CallbackBatchFSelect`

## Public fields

- `on_eval_after_design`:

  (`function()`)  
  Stage called after design is created. Called in
  `ObjectiveFSelectBatch$eval_many()`.

- `on_eval_after_benchmark`:

  (`function()`)  
  Stage called after feature sets are evaluated. Called in
  `ObjectiveFSelectBatch$eval_many()`.

- `on_eval_before_archive`:

  (`function()`)  
  Stage called before performance values are written to the archive.
  Called in `ObjectiveFSelectBatch$eval_many()`.

- `on_auto_fselector_before_final_model`:

  (`function()`)  
  Stage called before the final model is trained. Called in
  `AutoFSelector$train()`. This stage is called after the optimization
  has finished and the final model is trained with the best feature set
  found.

- `on_auto_fselector_after_final_model`:

  (`function()`)  
  Stage called after the final model is trained. Called in
  `AutoFSelector$train()`. This stage is called after the final model is
  trained with the best feature set found.

## Methods

### Public methods

- [`CallbackBatchFSelect$clone()`](#method-CallbackBatchFSelect-clone)

Inherited methods

- [`mlr3misc::Callback$call()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-call)
- [`mlr3misc::Callback$format()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-format)
- [`mlr3misc::Callback$help()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-help)
- [`mlr3misc::Callback$initialize()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-initialize)
- [`mlr3misc::Callback$print()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-print)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CallbackBatchFSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Write archive to disk
callback_batch_fselect("mlr3fselect.backup",
  on_optimization_end = function(callback, context) {
    saveRDS(context$instance$archive, "archive.rds")
  }
)
#> <CallbackBatchFSelect:mlr3fselect.backup>
#> * Active Stages: on_optimization_end
```
