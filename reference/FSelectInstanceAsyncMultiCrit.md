# Multi-Criteria Feature Selection with Rush

The `FSelectInstanceAsyncMultiCrit` specifies a feature selection
problem for a
[FSelectorAsync](https://mlr3fselect.mlr-org.com/reference/FSelectorAsync.md).
The function
[`fsi_async()`](https://mlr3fselect.mlr-org.com/reference/fsi_async.md)
creates a FSelectInstanceAsyncMultiCrit and the function
[`fselect()`](https://mlr3fselect.mlr-org.com/reference/fselect.md)
creates an instance internally.

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

## Analysis

For analyzing the feature selection results, it is recommended to pass
the
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/ArchiveAsyncFSelect.md)
to `as.data.table()`. The returned data table contains the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each feature subset evaluation.

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

## Super classes

[`bbotk::OptimInstance`](https://bbotk.mlr-org.com/reference/OptimInstance.html)
-\>
[`bbotk::OptimInstanceAsync`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html)
-\>
[`bbotk::OptimInstanceAsyncMultiCrit`](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncMultiCrit.html)
-\> `FSelectInstanceAsyncMultiCrit`

## Methods

### Public methods

- [`FSelectInstanceAsyncMultiCrit$new()`](#method-FSelectInstanceAsyncMultiCrit-new)

- [`FSelectInstanceAsyncMultiCrit$assign_result()`](#method-FSelectInstanceAsyncMultiCrit-assign_result)

- [`FSelectInstanceAsyncMultiCrit$clone()`](#method-FSelectInstanceAsyncMultiCrit-clone)

Inherited methods

- [`bbotk::OptimInstance$format()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-format)
- [`bbotk::OptimInstanceAsync$clear()`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html#method-clear)
- [`bbotk::OptimInstanceAsync$print()`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html#method-print)
- [`bbotk::OptimInstanceAsync$reconnect()`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html#method-reconnect)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectInstanceAsyncMultiCrit$new(
      task,
      learner,
      resampling,
      measures,
      terminator,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      rush = NULL
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

- `rush`:

  (`Rush`)  
  If a rush instance is supplied, the optimization runs without batches.

------------------------------------------------------------------------

### Method `assign_result()`

The
[FSelectorAsync](https://mlr3fselect.mlr-org.com/reference/FSelectorAsync.md)
object writes the best found points and estimated performance values
here (probably the Pareto set / front). For internal use.

#### Usage

    FSelectInstanceAsyncMultiCrit$assign_result(xdt, ydt, extra = NULL, ...)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  x values as `data.table`. Each row is one point. Contains the value in
  the *search space* of the
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md)
  object. Can contain additional columns for extra information.

- `ydt`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Optimal outcomes, e.g. the Pareto front.

- `extra`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Additional information.

- `...`:

  (`any`)  
  ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectInstanceAsyncMultiCrit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
