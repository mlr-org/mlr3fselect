# Class for Single Criterion Feature Selection

The FSelectInstanceBatchSingleCrit specifies a feature selection problem
for a
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md).
The function
[`fsi()`](https://mlr3fselect.mlr-org.com/dev/reference/fsi.md) creates
a FSelectInstanceBatchSingleCrit and the function
[`fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/fselect.md)
creates an instance internally.

The instance contains an
[ObjectiveFSelectBatch](https://mlr3fselect.mlr-org.com/dev/reference/ObjectiveFSelectBatch.md)
object that encodes the black box objective function a
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
has to optimize. The instance allows the basic operations of querying
the objective at design points (`$eval_batch()`). This operation is
usually done by the
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md).
Evaluations of feature subsets are performed in batches by calling
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
internally. The evaluated feature subsets are stored in the
[Archive](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveBatchFSelect.md)
(`$archive`). Before a batch is evaluated, the
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
is queried for the remaining budget. If the available budget is
exhausted, an exception is raised, and no further evaluations can be
performed from this point on. The
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
is also supposed to store its final result, consisting of a selected
feature subset and associated estimated performance values, by calling
the method `instance$assign_result()`.

## Default Measures

If no measure is passed, the default measure is used. The default
measure depends on the task type.

|                |                  |                                                               |
|----------------|------------------|---------------------------------------------------------------|
| Task           | Default Measure  | Package                                                       |
| `"classif"`    | `"classif.ce"`   | [mlr3](https://CRAN.R-project.org/package=mlr3)               |
| `"regr"`       | `"regr.mse"`     | [mlr3](https://CRAN.R-project.org/package=mlr3)               |
| `"surv"`       | `"surv.cindex"`  | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)     |
| `"dens"`       | `"dens.logloss"` | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)     |
| `"classif_st"` | `"classif.ce"`   | [mlr3spatial](https://CRAN.R-project.org/package=mlr3spatial) |
| `"regr_st"`    | `"regr.mse"`     | [mlr3spatial](https://CRAN.R-project.org/package=mlr3spatial) |
| `"clust"`      | `"clust.dunn"`   | [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) |

## Resources

There are several sections about feature selection in the
[mlr3book](https://mlr3book.mlr-org.com).

- Getting started with [wrapper feature
  selection](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-fs-wrapper).

- Do a [sequential forward
  selection](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-fs-wrapper-example)
  Palmer Penguins data set.

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

- Utilize the built-in feature importance of models with [Recursive
  Feature
  Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).

- Run a feature selection with [Shadow Variable
  Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).

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
[`bbotk::OptimInstanceBatchSingleCrit`](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
-\> `FSelectInstanceBatchSingleCrit`

## Active bindings

- `result_feature_set`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Feature set for task subsetting.

## Methods

### Public methods

- [`FSelectInstanceBatchSingleCrit$new()`](#method-FSelectInstanceBatchSingleCrit-new)

- [`FSelectInstanceBatchSingleCrit$assign_result()`](#method-FSelectInstanceBatchSingleCrit-assign_result)

- [`FSelectInstanceBatchSingleCrit$print()`](#method-FSelectInstanceBatchSingleCrit-print)

- [`FSelectInstanceBatchSingleCrit$clone()`](#method-FSelectInstanceBatchSingleCrit-clone)

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

    FSelectInstanceBatchSingleCrit$new(
      task,
      learner,
      resampling,
      measure,
      terminator,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      ties_method = "least_features"
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

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure to optimize. If `NULL`, default measure is used.

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
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackBatchFSelect.md))  
  List of callbacks.

- `ties_method`:

  (`character(1)`)  
  The method to break ties when selecting sets while optimizing and when
  selecting the best set. Can be `"least_features"` or `"random"`. The
  option `"least_features"` (default) selects the feature set with the
  least features. If there are multiple best feature sets with the same
  number of features, one is selected randomly. The `random` method
  returns a random feature set from the best feature sets. Ignored if
  multiple measures are used.

------------------------------------------------------------------------

### Method `assign_result()`

The
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
writes the best found feature subset and estimated performance value
here. For internal use.

#### Usage

    FSelectInstanceBatchSingleCrit$assign_result(xdt, y, extra = NULL, ...)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  x values as `data.table`. Each row is one point. Contains the value in
  the *search space* of the
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)
  object. Can contain additional columns for extra information.

- `y`:

  (`numeric(1)`)  
  Optimal outcome.

- `extra`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Additional information.

- `...`:

  (`any`)  
  ignored.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    FSelectInstanceBatchSingleCrit$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectInstanceBatchSingleCrit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Feature selection on Palmer Penguins data set
# \donttest{

task = tsk("penguins")
learner = lrn("classif.rpart")

# Construct feature selection instance
instance = fsi(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("evals", n_evals = 4)
)

# Choose optimization algorithm
fselector = fs("random_search", batch_size = 2)

# Run feature selection
fselector$optimize(instance)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE  FALSE
#>                                                      features n_features
#>                                                        <list>      <int>
#> 1: bill_depth,bill_length,body_mass,flipper_length,island,sex          6
#>    classif.ce
#>         <num>
#> 1: 0.06112382

# Subset task to optimal feature set
task$select(instance$result_feature_set)

# Train the learner with optimal feature set on the full data set
learner$train(task)

# Inspect all evaluated sets
as.data.table(instance$archive)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE       FALSE     FALSE           TRUE   TRUE   TRUE   TRUE
#> 2:      FALSE       FALSE     FALSE           TRUE  FALSE   TRUE   TRUE
#> 3:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE  FALSE
#> 4:       TRUE       FALSE     FALSE           TRUE  FALSE  FALSE   TRUE
#>    classif.ce runtime_learners           timestamp batch_nr warnings errors
#>         <num>            <num>              <POSc>    <int>    <int>  <int>
#> 1: 0.15400458            0.019 2026-04-14 12:15:17        1        0      0
#> 2: 0.19768624            0.014 2026-04-14 12:15:17        1        0      0
#> 3: 0.06112382            0.017 2026-04-14 12:15:17        2        0      0
#> 4: 0.18893974            0.017 2026-04-14 12:15:17        2        0      0
#>                                                      features n_features
#>                                                        <list>     <list>
#> 1:                  bill_depth,flipper_length,island,sex,year          5
#> 2:                                    flipper_length,sex,year          3
#> 3: bill_depth,bill_length,body_mass,flipper_length,island,sex          6
#> 4:                             bill_depth,flipper_length,year          3
#>     resample_result
#>              <list>
#> 1: <ResampleResult>
#> 2: <ResampleResult>
#> 3: <ResampleResult>
#> 4: <ResampleResult>
# }
```
