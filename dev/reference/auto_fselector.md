# Function for Automatic Feature Selection

The
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
wraps a [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
and augments it with an automatic feature selection. The
`auto_fselector()` function creates an
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
object.

## Usage

``` r
auto_fselector(
  fselector,
  learner,
  resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  store_fselect_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features",
  rush = NULL,
  id = NULL
)
```

## Arguments

- fselector:

  ([FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md))  
  Optimization algorithm.

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to optimize the feature subset for.

- resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling that is used to evaluated the performance of the feature
  subsets. Uninstantiated resamplings are instantiated during
  construction so that all feature subsets are evaluated on the same
  data splits. Already instantiated resamplings are kept unchanged.

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
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
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

- id:

  (`character(1)`)  
  Identifier for the new instance.

## Value

[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md).

## Details

The
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
is a [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
which wraps another
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
performs the following steps during `$train()`:

1.  The wrapped (inner) learner is trained on the feature subsets via
    resampling. The feature selection can be specified by providing a
    [FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
    a
    [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html),
    a
    [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html),
    and a
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html).

2.  A final model is fit on the complete training data with the
    best-found feature subset.

During `$predict()` the
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
just calls the predict method of the wrapped (inner) learner.

## Resources

There are several sections about feature selection in the
[mlr3book](https://mlr3book.mlr-org.com).

- Estimate Model Performance with [nested
  resampling](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-autofselect).

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

## Nested Resampling

Nested resampling can be performed by passing an
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
object to
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html)
or
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html).
To access the inner resampling results, set
`store_fselect_instance = TRUE` and execute
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html)
or
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
with `store_models = TRUE` (see examples). The
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
passed to the
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
is meant to be the inner resampling, operating on the training set of an
arbitrary outer resampling. For this reason it is not feasible to pass
an instantiated
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
here.

## Examples

``` r
afs = auto_fselector(
  fselector = fs("random_search"),
  learner = lrn("classif.rpart"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

afs$train(tsk("pima"))
```
