# Freeze Archive Callback

This
[CallbackAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackAsyncFSelect.md)
freezes the
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/ArchiveAsyncFSelect.md)
to
[ArchiveAsyncFSelectFrozen](https://mlr3fselect.mlr-org.com/reference/ArchiveAsyncFSelectFrozen.md)
after the optimization has finished.

## Examples

``` r
clbk("mlr3fselect.async_freeze_archive")
#> <CallbackAsyncFSelect:mlr3fselect.async_freeze_archive>: Archive Freeze Callback
#> * Active Stages: on_optimization_end
```
