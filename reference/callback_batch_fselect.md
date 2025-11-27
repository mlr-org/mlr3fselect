# Create Feature Selection Callback

Function to create a
[CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackBatchFSelect.md).
Predefined callbacks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html).

Feature selection callbacks can be called from different stages of
feature selection. The stages are prefixed with `on_*`. The
`on_auto_fselector_*` stages are only available when the callback is
used in an
[AutoFSelector](https://mlr3fselect.mlr-org.com/reference/AutoFSelector.md).

    Start Automatic Feature Selection
      Start Feature Selection
          - on_optimization_begin
          Start FSelect Batch
              - on_optimizer_before_eval
              Start Evaluation
                  - on_eval_after_design
                  - on_eval_after_benchmark
                  - on_eval_before_archive
              End Evaluation
              - on_optimizer_after_eval
          End FSelect Batch
          - on_result
          - on_optimization_end
      End Feature Selection
      - on_auto_fselector_before_final_model
      - on_auto_fselector_after_final_model
    End Automatic Feature Selection

See also the section on parameters for more information on the stages. A
feature selection callback works with
[bbotk::ContextBatch](https://bbotk.mlr-org.com/reference/ContextBatch.html)
and
[ContextBatchFSelect](https://mlr3fselect.mlr-org.com/reference/ContextBatchFSelect.md).

## Usage

``` r
callback_batch_fselect(
  id,
  label = NA_character_,
  man = NA_character_,
  on_optimization_begin = NULL,
  on_optimizer_before_eval = NULL,
  on_eval_after_design = NULL,
  on_eval_after_benchmark = NULL,
  on_eval_before_archive = NULL,
  on_optimizer_after_eval = NULL,
  on_result = NULL,
  on_optimization_end = NULL,
  on_auto_fselector_before_final_model = NULL,
  on_auto_fselector_after_final_model = NULL
)
```

## Arguments

- id:

  (`character(1)`)  
  Identifier for the new instance.

- label:

  (`character(1)`)  
  Label for the new instance.

- man:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

- on_optimization_begin:

  (`function()`)  
  Stage called at the beginning of the optimization. Called in
  `Optimizer$optimize()`.

- on_optimizer_before_eval:

  (`function()`)  
  Stage called after the optimizer proposes points. Called in
  `OptimInstance$eval_batch()`.

- on_eval_after_design:

  (`function()`)  
  Stage called after design is created. Called in
  `ObjectiveFSelectBatch$eval_many()`.

- on_eval_after_benchmark:

  (`function()`)  
  Stage called after feature sets are evaluated. Called in
  `ObjectiveFSelectBatch$eval_many()`.

- on_eval_before_archive:

  (`function()`)  
  Stage called before performance values are written to the archive.
  Called in `ObjectiveFSelectBatch$eval_many()`.

- on_optimizer_after_eval:

  (`function()`)  
  Stage called after points are evaluated. Called in
  `OptimInstance$eval_batch()`.

- on_result:

  (`function()`)  
  Stage called after result are written. Called in
  `OptimInstance$assign_result()`.

- on_optimization_end:

  (`function()`)  
  Stage called at the end of the optimization. Called in
  `Optimizer$optimize()`.

- on_auto_fselector_before_final_model:

  (`function()`)  
  Stage called before the final model is trained. Called in
  `AutoFSelector$train()`.

- on_auto_fselector_after_final_model:

  (`function()`)  
  Stage called after the final model is trained. Called in
  `AutoFSelector$train()`.

## Details

When implementing a callback, each function must have two arguments
named `callback` and `context`. A callback can write data to the state
(`$state`), e.g. settings that affect the callback itself. Avoid writing
large data the state.

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
