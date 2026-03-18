# Backup Benchmark Result Callback

This
[CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackBatchFSelect.md)
writes the
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
after each batch to disk.

## Examples

``` r
clbk("mlr3fselect.backup", path = "backup.rds")
#> <CallbackBatchFSelect:mlr3fselect.backup>: Backup Benchmark Result Callback
#> * Active Stages: on_optimizer_after_eval, on_optimization_begin

# Run feature selection on the Palmer Penguins data set
instance = fselect(
  fselector = fs("random_search"),
  task = tsk("pima"),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("holdout"),
  measures = msr("classif.ce"),
  term_evals = 4,
  callbacks = clbk("mlr3fselect.backup", path = tempfile(fileext = ".rds")))
```
