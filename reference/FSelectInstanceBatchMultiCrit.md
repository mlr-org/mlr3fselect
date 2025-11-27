# Class for Multi Criteria Feature Selection

The FSelectInstanceBatchMultiCrit specifies a feature selection problem
for a
[FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md). The
function [`fsi()`](https://mlr3fselect.mlr-org.com/reference/fsi.md)
creates a FSelectInstanceBatchMultiCrit and the function
[`fselect()`](https://mlr3fselect.mlr-org.com/reference/fselect.md)
creates an instance internally.

## Resources

There are several sections about feature selection in the
[mlr3book](https://mlr3book.mlr-org.com).

- Learn about [multi-objective
  optimization](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-multicrit-featsel).

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

## Analysis

For analyzing the feature selection results, it is recommended to pass
the archive to `as.data.table()`. The returned data table is joined with
the benchmark result which adds the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each feature set.

The archive provides various getters (e.g. `$learners()`) to ease the
access. All getters extract by position (`i`) or unique hash (`uhash`).
For a complete list of all getters see the methods section.

The benchmark result (`$benchmark_result`) allows to score the feature
sets again on a different measure. Alternatively, measures can be
supplied to `as.data.table()`.

## Super classes

[`bbotk::OptimInstance`](https://bbotk.mlr-org.com/reference/OptimInstance.html)
-\>
[`bbotk::OptimInstanceBatch`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
-\>
[`bbotk::OptimInstanceBatchMultiCrit`](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html)
-\> `FSelectInstanceBatchMultiCrit`

## Active bindings

- `result_feature_set`:

  (list of [`character()`](https://rdrr.io/r/base/character.html))  
  Feature sets for task subsetting.

## Methods

### Public methods

- [`FSelectInstanceBatchMultiCrit$new()`](#method-FSelectInstanceBatchMultiCrit-new)

- [`FSelectInstanceBatchMultiCrit$assign_result()`](#method-FSelectInstanceBatchMultiCrit-assign_result)

- [`FSelectInstanceBatchMultiCrit$print()`](#method-FSelectInstanceBatchMultiCrit-print)

- [`FSelectInstanceBatchMultiCrit$clone()`](#method-FSelectInstanceBatchMultiCrit-clone)

Inherited methods

- [`bbotk::OptimInstance$clear()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-clear)
- [`bbotk::OptimInstance$format()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-format)
- [`bbotk::OptimInstanceBatch$eval_batch()`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html#method-eval_batch)
- [`bbotk::OptimInstanceBatch$objective_function()`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html#method-objective_function)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectInstanceBatchMultiCrit$new(
      task,
      learner,
      resampling,
      measures,
      terminator,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to optimize the feature subset for.

- `resampling`:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling that is used to evaluated the performance of the feature
  subsets. Uninstantiated resamplings are instantiated during
  construction so that all feature subsets are evaluated on the same
  data splits. Already instantiated resamplings are kept unchanged.

- `measures`:

  (list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measures to optimize. If `NULL`,
  [mlr3](https://CRAN.R-project.org/package=mlr3)'s default measure is
  used.

- `terminator`:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the feature selection.

- `store_benchmark_result`:

  (`logical(1)`)  
  Store benchmark result in archive?

- `store_models`:

  (`logical(1)`). Store models in benchmark result?

- `check_values`:

  (`logical(1)`)  
  Check the parameters before the evaluation and the results for
  validity?

- `callbacks`:

  (list of
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackBatchFSelect.md))  
  List of callbacks.

------------------------------------------------------------------------

### Method `assign_result()`

The [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
object writes the best found feature subsets and estimated performance
values here. For internal use.

#### Usage

    FSelectInstanceBatchMultiCrit$assign_result(xdt, ydt, extra = NULL, ...)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  x values as `data.table`. Each row is one point. Contains the value in
  the *search space* of the FSelectInstanceBatchMultiCrit object. Can
  contain additional columns for extra information.

- `ydt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Optimal outcomes, e.g. the Pareto front.

- `extra`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Additional information.

- `...`:

  (`any`)  
  ignored.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    FSelectInstanceBatchMultiCrit$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectInstanceBatchMultiCrit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Feature selection on Palmer Penguins data set
# \donttest{

task = tsk("penguins")

# Construct feature selection instance
instance = fsi(
  task = task,
  learner = lrn("classif.rpart"),
  resampling = rsmp("cv", folds = 3),
  measures = msrs(c("classif.ce", "time_train")),
  terminator = trm("evals", n_evals = 4)
)

# Choose optimization algorithm
fselector = fs("random_search", batch_size = 2)

# Run feature selection
fselector$optimize(instance)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#> 2:      FALSE       FALSE     FALSE          FALSE  FALSE   TRUE  FALSE
#> 3:       TRUE        TRUE      TRUE          FALSE  FALSE  FALSE   TRUE
#>                                 features n_features classif.ce  time_train
#>                                   <list>      <int>      <num>       <num>
#> 1:                             body_mass          1 0.27887109 0.002666667
#> 2:                                   sex          1 0.55797101 0.002333333
#> 3: bill_depth,bill_length,body_mass,year          1 0.07556573 0.003000000

# Optimal feature sets
instance$result_feature_set
#> [[1]]
#> [1] "body_mass"
#> 
#> [[2]]
#> [1] "sex"
#> 
#> [[3]]
#> [1] "bill_depth"  "bill_length" "body_mass"   "year"       
#> 

# Inspect all evaluated sets
as.data.table(instance$archive)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE       FALSE      TRUE           TRUE   TRUE  FALSE   TRUE
#> 2:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#> 3:      FALSE       FALSE     FALSE          FALSE  FALSE   TRUE  FALSE
#> 4:       TRUE        TRUE      TRUE          FALSE  FALSE  FALSE   TRUE
#>    classif.ce  time_train runtime_learners           timestamp batch_nr
#>         <num>       <num>            <num>              <POSc>    <int>
#> 1: 0.17165014 0.003333333            0.016 2025-11-27 11:00:52        1
#> 2: 0.27887109 0.002666667            0.013 2025-11-27 11:00:52        1
#> 3: 0.55797101 0.002333333            0.013 2025-11-27 11:00:53        2
#> 4: 0.07556573 0.003000000            0.015 2025-11-27 11:00:53        2
#>    warnings errors                                        features n_features
#>       <int>  <int>                                          <list>     <list>
#> 1:        0      0 bill_depth,body_mass,flipper_length,island,year          5
#> 2:        0      0                                       body_mass          1
#> 3:        0      0                                             sex          1
#> 4:        0      0           bill_depth,bill_length,body_mass,year          4
#>     resample_result
#>              <list>
#> 1: <ResampleResult>
#> 2: <ResampleResult>
#> 3: <ResampleResult>
#> 4: <ResampleResult>
# }
```
