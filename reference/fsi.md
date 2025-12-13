# Syntactic Sugar for Feature Selection Instance Construction

Function to construct a
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
or
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md).

## Usage

``` r
fsi(
  task,
  learner,
  resampling,
  measures = NULL,
  terminator,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features"
)
```

## Arguments

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to optimize the feature subset for.

- resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling that is used to evaluated the performance of the feature
  subsets. Uninstantiated resamplings are instantiated during
  construction so that all feature subsets are evaluated on the same
  data splits. Already instantiated resamplings are kept unchanged.

- measures:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) or
  list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  A single measure creates a
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
  and multiple measures a
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md).
  If `NULL`, default measure is used.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the feature selection.

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
#> 1:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#>              features n_features classif.ce
#>                <list>      <int>      <num>
#> 1: bill_length,island          2 0.05235189

# Subset task to optimal feature set
task$select(instance$result_feature_set)

# Train the learner with optimal feature set on the full data set
learner$train(task)

# Inspect all evaluated sets
as.data.table(instance$archive)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 2:       TRUE        TRUE      TRUE          FALSE  FALSE   TRUE  FALSE
#> 3:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 4:       TRUE        TRUE     FALSE          FALSE  FALSE   TRUE   TRUE
#>    classif.ce runtime_learners           timestamp batch_nr warnings errors
#>         <num>            <num>              <POSc>    <int>    <int>  <int>
#> 1: 0.06104755            0.014 2025-12-13 15:56:36        1        0      0
#> 2: 0.06976862            0.014 2025-12-13 15:56:36        1        0      0
#> 3: 0.05235189            0.015 2025-12-13 15:56:36        2        0      0
#> 4: 0.09300788            0.017 2025-12-13 15:56:36        2        0      0
#>                                features n_features  resample_result
#>                                  <list>     <list>           <list>
#> 1:        bill_depth,bill_length,island          3 <ResampleResult>
#> 2: bill_depth,bill_length,body_mass,sex          4 <ResampleResult>
#> 3:                   bill_length,island          2 <ResampleResult>
#> 4:      bill_depth,bill_length,sex,year          4 <ResampleResult>
# }
```
