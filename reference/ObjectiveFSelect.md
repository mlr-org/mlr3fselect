# Class for Feature Selection Objective

Stores the objective function that estimates the performance of feature
subsets. This class is usually constructed internally by the
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
/
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md).

## Super class

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\> `ObjectiveFSelect`

## Public fields

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)).

- `resampling`:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)).

- `measures`:

  (list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `store_models`:

  (`logical(1)`).

- `store_benchmark_result`:

  (`logical(1)`).

- `callbacks`:

  (List of
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackBatchFSelect.md)s).

## Methods

### Public methods

- [`ObjectiveFSelect$new()`](#method-ObjectiveFSelect-new)

- [`ObjectiveFSelect$clone()`](#method-ObjectiveFSelect-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$eval_dt()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_dt)
- [`bbotk::Objective$eval_many()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_many)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ObjectiveFSelect$new(
      task,
      learner,
      resampling,
      measures,
      check_values = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
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

- `check_values`:

  (`logical(1)`)  
  Check the parameters before the evaluation and the results for
  validity?

- `store_benchmark_result`:

  (`logical(1)`)  
  Store benchmark result in archive?

- `store_models`:

  (`logical(1)`). Store models in benchmark result?

- `callbacks`:

  (list of
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/reference/CallbackBatchFSelect.md))  
  List of callbacks.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObjectiveFSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
