# Internal Tuning Callback

This callback runs internal tuning alongside the feature selection. The
internal tuning values are aggregated and stored in the results. The
final model is trained with the best feature set and the tuned value.

## Examples

``` r
clbk("mlr3fselect.internal_tuning")
#> <CallbackBatchFSelect:mlr3fselect.internal_tuning>: Internal Tuning
#> * Active Stages: on_auto_fselector_after_final_model,
#>   on_auto_fselector_before_final_model, on_eval_before_archive,
#>   on_optimization_end
```
