# Class for Automatic Feature Selection

The AutoFSelector wraps a
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
augments it with an automatic feature selection. The
[`auto_fselector()`](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md)
function creates an AutoFSelector object.

## Details

The AutoFSelector is a
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) which
wraps another
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
performs the following steps during `$train()`:

1.  The wrapped (inner) learner is trained on the feature subsets via
    resampling. The feature selection can be specified by providing a
    [FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
    a
    [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html),
    a
    [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
    and a
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html).

2.  A final model is fit on the complete training data with the
    best-found feature subset.

During `$predict()` the AutoFSelector just calls the predict method of
the wrapped (inner) learner.

## Resources

There are several sections about feature selection in the
[mlr3book](https://mlr3book.mlr-org.com).

- Estimate Model Performance with [nested
  resampling](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-autofselect).

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

## Nested Resampling

Nested resampling can be performed by passing an AutoFSelector object to
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
passed to the AutoFSelector is meant to be the inner resampling,
operating on the training set of an arbitrary outer resampling. For this
reason it is not feasible to pass an instantiated
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
here.

## Super class

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
`AutoFSelector`

## Public fields

- `instance_args`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  All arguments from construction to create the
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md).

- `fselector`:

  ([FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md))  
  Optimization algorithm.

## Active bindings

- `archive`:

  (\[ArchiveBatchFSelect)  
  Returns
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
  archive.

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Trained learner.

- `fselect_instance`:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md))  
  Internally created feature selection instance with all intermediate
  results.

- `fselect_result`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Short-cut to `$result` from
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md).

- `predict_type`:

  (`character(1)`)  
  Stores the currently active predict type, e.g. `"response"`. Must be
  an element of `$predict_types`.

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object.

- `phash`:

  (`character(1)`)  
  Hash (unique identifier) for this partial object, excluding some
  components which are varied systematically during tuning (parameter
  values) or feature selection (feature names).

## Methods

### Public methods

- [`AutoFSelector$new()`](#method-AutoFSelector-new)

- [`AutoFSelector$base_learner()`](#method-AutoFSelector-base_learner)

- [`AutoFSelector$importance()`](#method-AutoFSelector-importance)

- [`AutoFSelector$selected_features()`](#method-AutoFSelector-selected_features)

- [`AutoFSelector$oob_error()`](#method-AutoFSelector-oob_error)

- [`AutoFSelector$loglik()`](#method-AutoFSelector-loglik)

- [`AutoFSelector$print()`](#method-AutoFSelector-print)

- [`AutoFSelector$clone()`](#method-AutoFSelector-clone)

Inherited methods

- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoFSelector$new(
      fselector,
      learner,
      resampling,
      measure = NULL,
      terminator,
      store_fselect_instance = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      ties_method = "least_features",
      rush = NULL,
      id = NULL
    )

#### Arguments

- `fselector`:

  ([FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md))  
  Optimization algorithm.

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

- `store_fselect_instance`:

  (`logical(1)`)  
  If `TRUE` (default), stores the internally created
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
  with all intermediate results in slot `$fselect_instance`. Is set to
  `TRUE`, if `store_models = TRUE`

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

- `rush`:

  (`Rush`)  
  If a rush instance is supplied, the optimization runs without batches.

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `base_learner()`

Extracts the base learner from nested learner objects like
`GraphLearner` in
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines). If
`recursive = 0`, the (tuned) learner is returned.

#### Usage

    AutoFSelector$base_learner(recursive = Inf)

#### Arguments

- `recursive`:

  (`integer(1)`)  
  Depth of recursion for multiple nested objects.

#### Returns

[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html).

------------------------------------------------------------------------

### Method `importance()`

The importance scores of the final model.

#### Usage

    AutoFSelector$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

The selected features of the final model. These features are selected
internally by the learner.

#### Usage

    AutoFSelector$selected_features()

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `oob_error()`

The out-of-bag error of the final model.

#### Usage

    AutoFSelector$oob_error()

#### Returns

`numeric(1)`.

------------------------------------------------------------------------

### Method `loglik()`

The log-likelihood of the final model.

#### Usage

    AutoFSelector$loglik()

#### Returns

`logLik`. Printer.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

#### Usage

    AutoFSelector$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoFSelector$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Automatic Feature Selection
# \donttest{

# split to train and external set
task = tsk("penguins")
split = partition(task, ratio = 0.8)

# create auto fselector
afs = auto_fselector(
  fselector = fs("random_search"),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

# optimize feature subset and fit final model
afs$train(task, row_ids = split$train)

# predict with final model
afs$predict(task, row_ids = split$test)
#> 
#> ── <PredictionClassif> for 69 observations: ────────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        9    Adelie    Adelie
#>      ---       ---       ---
#>      318 Chinstrap Chinstrap
#>      334 Chinstrap Chinstrap
#>      338 Chinstrap Chinstrap

# show result
afs$fselect_result
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE   TRUE
#>                       features n_features classif.ce
#>                         <list>      <int>      <num>
#> 1: bill_length,island,sex,year          4 0.06521739

# model slot contains trained learner and fselect instance
afs$model
#> $learner
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: rpart
#> • Parameters: xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use', predict_raw = 'FALSE'
#> 
#> $features
#> [1] "bill_length" "island"      "sex"         "year"       
#> 
#> $fselect_instance
#> 
#> ── <FSelectInstanceBatchSingleCrit> ────────────────────────────────────────────
#> • State: Optimized
#> • Objective: <ObjectiveFSelectBatch> (classif.rpart_on_penguins)
#> • Terminator: <TerminatorEvals>
#> • Result:
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE   TRUE
#>    classif.ce
#>         <num>
#> 1: 0.06521739
#> • Archive:
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  2:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE   TRUE
#>  3:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  4:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  5:       TRUE        TRUE      TRUE           TRUE  FALSE   TRUE   TRUE
#>  6:      FALSE        TRUE      TRUE           TRUE  FALSE  FALSE   TRUE
#>  7:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  8:       TRUE        TRUE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  9:       TRUE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 10:       TRUE       FALSE      TRUE          FALSE  FALSE   TRUE  FALSE
#>     classif.ce
#>          <num>
#>  1: 0.09782609
#>  2: 0.06521739
#>  3: 0.25000000
#>  4: 0.09782609
#>  5: 0.09782609
#>  6: 0.09782609
#>  7: 0.09782609
#>  8: 0.07608696
#>  9: 0.29347826
#> 10: 0.20652174
#> 

# shortcut trained learner
afs$learner
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: rpart
#> • Parameters: xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use', predict_raw = 'FALSE'

# shortcut fselect instance
afs$fselect_instance
#> 
#> ── <FSelectInstanceBatchSingleCrit> ────────────────────────────────────────────
#> • State: Optimized
#> • Objective: <ObjectiveFSelectBatch> (classif.rpart_on_penguins)
#> • Terminator: <TerminatorEvals>
#> • Result:
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE   TRUE
#>    classif.ce
#>         <num>
#> 1: 0.06521739
#> • Archive:
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  2:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE   TRUE
#>  3:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  4:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  5:       TRUE        TRUE      TRUE           TRUE  FALSE   TRUE   TRUE
#>  6:      FALSE        TRUE      TRUE           TRUE  FALSE  FALSE   TRUE
#>  7:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  8:       TRUE        TRUE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  9:       TRUE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 10:       TRUE       FALSE      TRUE          FALSE  FALSE   TRUE  FALSE
#>     classif.ce
#>          <num>
#>  1: 0.09782609
#>  2: 0.06521739
#>  3: 0.25000000
#>  4: 0.09782609
#>  5: 0.09782609
#>  6: 0.09782609
#>  7: 0.09782609
#>  8: 0.07608696
#>  9: 0.29347826
#> 10: 0.20652174


# Nested Resampling

afs = auto_fselector(
  fselector = fs("random_search"),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

resampling_outer = rsmp("cv", folds = 3)
rr = resample(task, afs, resampling_outer, store_models = TRUE)

# retrieve inner feature selection results.
extract_inner_fselect_results(rr)
#>    iteration bill_depth bill_length body_mass flipper_length island    sex
#>        <int>     <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl>
#> 1:         1       TRUE        TRUE     FALSE          FALSE   TRUE   TRUE
#> 2:         2      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE
#> 3:         3      FALSE        TRUE      TRUE           TRUE  FALSE  FALSE
#>      year classif.ce                                  features n_features
#>    <lgcl>      <num>                                    <list>      <int>
#> 1:  FALSE 0.09210526         bill_depth,bill_length,island,sex          4
#> 2:  FALSE 0.03947368                bill_length,flipper_length          2
#> 3:   TRUE 0.06493506 bill_length,body_mass,flipper_length,year          4
#>     task_id              learner_id resampling_id
#>      <char>                  <char>        <char>
#> 1: penguins classif.rpart.fselector            cv
#> 2: penguins classif.rpart.fselector            cv
#> 3: penguins classif.rpart.fselector            cv

# performance scores estimated on the outer resampling
rr$score()
#>     task_id              learner_id resampling_id iteration classif.ce
#>      <char>                  <char>        <char>     <int>      <num>
#> 1: penguins classif.rpart.fselector            cv         1 0.06086957
#> 2: penguins classif.rpart.fselector            cv         2 0.05217391
#> 3: penguins classif.rpart.fselector            cv         3 0.07894737
#> Hidden columns: task, learner, resampling, prediction_test

# unbiased performance of the final model trained on the full data set
rr$aggregate()
#> classif.ce 
#> 0.06399695 
# }
```
