# Function for Nested Resampling

Function to conduct nested resampling.

## Usage

``` r
fselect_nested(
  fselector,
  task,
  learner,
  inner_resampling,
  outer_resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  store_fselect_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features"
)
```

## Arguments

- fselector:

  ([FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md))  
  Optimization algorithm.

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to optimize the feature subset for.

- inner_resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling used for the inner loop.

- outer_resampling:

  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling used for the outer loop.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure to optimize. If `NULL`, default measure is used.

- term_evals:

  (`integer(1)`)  
  Number of allowed evaluations. Ignored if `terminator` is passed.

- term_time:

  (`integer(1)`)  
  Maximum allowed time in seconds. Ignored if `terminator` is passed.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the feature selection.

- store_fselect_instance:

  (`logical(1)`)  
  If `TRUE` (default), stores the internally created
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
  with all intermediate results in slot `$fselect_instance`. Is set to
  `TRUE`, if `store_models = TRUE`

- store_benchmark_result:

  (`logical(1)`)  
  Store benchmark result in archive?

- store_models:

  (`logical(1)`). Store models in benchmark result?

- check_values:

  (`logical(1)`)  
  Check the parameters before the evaluation and the results for
  validity?

- callbacks:

  (list of
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackBatchFSelect.md))  
  List of callbacks.

- ties_method:

  (`character(1)`)  
  The method to break ties when selecting sets while optimizing and when
  selecting the best set. Can be `"least_features"` or `"random"`. The
  option `"least_features"` (default) selects the feature set with the
  least features. If there are multiple best feature sets with the same
  number of features, one is selected randomly. The `random` method
  returns a random feature set from the best feature sets. Ignored if
  multiple measures are used.

## Value

[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)

## Examples

``` r
# Nested resampling on Palmer Penguins data set
rr = fselect_nested(
  fselector = fs("random_search"),
  task = tsk("penguins"),
  learner = lrn("classif.rpart"),
  inner_resampling = rsmp ("holdout"),
  outer_resampling = rsmp("cv", folds = 2),
  measure = msr("classif.ce"),
  term_evals = 4)

# Performance scores estimated on the outer resampling
rr$score()
#>     task_id              learner_id resampling_id iteration classif.ce
#>      <char>                  <char>        <char>     <int>      <num>
#> 1: penguins classif.rpart.fselector            cv         1 0.08139535
#> 2: penguins classif.rpart.fselector            cv         2 0.06395349
#> Hidden columns: task, learner, resampling, prediction_test

# Unbiased performance of the final model trained on the full data set
rr$aggregate()
#> classif.ce 
#> 0.07267442 
```
