# Function for Feature Selection

Function to optimize the features of a
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html). The
function internally creates a
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
or
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)
which describes the feature selection problem. It executes the feature
selection with the
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
(`fselector`) and returns the result with the feature selection instance
(`$result`). The
[ArchiveBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveBatchFSelect.md)
and
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveAsyncFSelect.md)
(`$archive`) stores all evaluated feature subsets and performance
scores.

You can find an overview of all feature selectors on our
[website](https://mlr-org.com/fselectors.html).

## Usage

``` r
fselect(
  fselector,
  task,
  learner,
  resampling,
  measures = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features",
  rush = NULL
)
```

## Arguments

- fselector:

  ([FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md))  
  Optimization algorithm.

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
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
  and multiple measures a
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md).
  If `NULL`, default measure is used.

- term_evals:

  (`integer(1)`)  
  Number of allowed evaluations. Ignored if `terminator` is passed.

- term_time:

  (`integer(1)`)  
  Maximum allowed time in seconds. Ignored if `terminator` is passed.

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
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackBatchFSelect.md))  
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

- rush:

  (`Rush`)  
  If a rush instance is supplied, the optimization runs without batches.

## Value

[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
\|
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)

## Details

The [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html),
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html),
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html),
[mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) and
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
are used to construct a
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md).
If multiple performance
[mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)s are
supplied, a
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)
is created. The parameter `term_evals` and `term_time` are shortcuts to
create a
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).
If both parameters are passed, a
[bbotk::TerminatorCombo](https://bbotk.mlr-org.com/reference/mlr_terminators_combo.html)
is constructed. For other
[Terminators](https://bbotk.mlr-org.com/reference/Terminator.html), pass
one with `terminator`. If no termination criterion is needed, set
`term_evals`, `term_time` and `terminator` to `NULL`.

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

## Examples

``` r
# Feature selection on the Pima Indians data set
task = tsk("pima")

# Load learner
learner = lrn("classif.rpart")

# Run feature selection
instance = fselect(
  fselector = fs("random_search", batch_size = 2),
  task = task,
  learner = learner,
  resampling = rsmp ("holdout"),
  measures = msr("classif.ce"),
  term_evals = 4)

# Subset task to optimized feature set
task$select(instance$result_feature_set)

# Train the learner with optimal feature set on the full data set
learner$train(task)

# Inspect all evaluated feature subsets
as.data.table(instance$archive)
#>       age glucose insulin   mass pedigree pregnant pressure triceps classif.ce
#>    <lgcl>  <lgcl>  <lgcl> <lgcl>   <lgcl>   <lgcl>   <lgcl>  <lgcl>      <num>
#> 1:  FALSE   FALSE    TRUE   TRUE    FALSE    FALSE    FALSE   FALSE  0.3632812
#> 2:  FALSE   FALSE   FALSE  FALSE     TRUE    FALSE     TRUE   FALSE  0.3710938
#> 3:   TRUE   FALSE   FALSE   TRUE     TRUE     TRUE     TRUE   FALSE  0.3476562
#> 4:   TRUE   FALSE   FALSE   TRUE     TRUE     TRUE    FALSE    TRUE  0.3710938
#>    runtime_learners           timestamp batch_nr warnings errors
#>               <num>              <POSc>    <int>    <int>  <int>
#> 1:            0.007 2026-04-14 12:31:22        1        0      0
#> 2:            0.008 2026-04-14 12:31:22        1        0      0
#> 3:            0.009 2026-04-14 12:31:22        2        0      0
#> 4:            0.009 2026-04-14 12:31:22        2        0      0
#>                               features n_features  resample_result
#>                                 <list>     <list>           <list>
#> 1:                        insulin,mass          2 <ResampleResult>
#> 2:                   pedigree,pressure          2 <ResampleResult>
#> 3: age,mass,pedigree,pregnant,pressure          5 <ResampleResult>
#> 4:  age,mass,pedigree,pregnant,triceps          5 <ResampleResult>
```
