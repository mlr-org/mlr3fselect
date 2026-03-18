# Ensemble Feature Selection Result

The `EnsembleFSResult` stores the results of ensemble feature selection.
It includes methods for evaluating the stability of the feature
selection process and for ranking the selected features among others.

Both functions
[`ensemble_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fselect.md)
and
[`embedded_ensemble_fselect()`](https://mlr3fselect.mlr-org.com/dev/reference/embedded_ensemble_fselect.md)
return an object of this class.

## S3 Methods

- `as.data.table.EnsembleFSResult(x, benchmark_result = TRUE)`  
  Returns a tabular view of the ensemble feature selection.  
  EnsembleFSResult -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  

  - `x` (EnsembleFSResult)

  - `benchmark_result` (`logical(1)`)  
    Whether to add the learner, task and resampling information from the
    benchmark result.

- `c(...)`  
  (EnsembleFSResult, ...) -\> EnsembleFSResult  
  Combines multiple EnsembleFSResult objects into a new
  EnsembleFSResult.

## References

Das, I (1999). “On characterizing the 'knee' of the Pareto curve based
on normal-boundary intersection.” *Structural Optimization*,
**18**(1-2), 107–115. ISSN 09344373.

Meinshausen, Nicolai, Buhlmann, Peter (2010). “Stability Selection.”
*Journal of the Royal Statistical Society Series B: Statistical
Methodology*, **72**(4), 417–473. ISSN 1369-7412,
[doi:10.1111/J.1467-9868.2010.00740.X](https://doi.org/10.1111/J.1467-9868.2010.00740.X)
, 0809.2932.

## Public fields

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  The benchmark result.

- `man`:

  (`character(1)`)  
  Manual page for this object.

## Active bindings

- `result`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns the result of the ensemble feature selection.

- `n_learners`:

  (`numeric(1)`)  
  Returns the number of learners used in the ensemble feature selection.

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Returns the 'active' measure that is used in methods of this object.

- `active_measure`:

  (`character(1)`)  
  Indicates the type of the active performance measure.

  During the ensemble feature selection process, the dataset is split
  into **multiple subsamples** (train/test splits) using an initial
  resampling scheme. So, performance can be evaluated using one of two
  measures:

  - `"outer"`: measure used to evaluate the performance on the test
    sets.

  - `"inner"`: measure used for optimization and to compute performance
    during inner resampling on the training sets.

- `n_resamples`:

  (`character(1)`)  
  Returns the number of times the task was initially resampled in the
  ensemble feature selection process.

## Methods

### Public methods

- [`EnsembleFSResult$new()`](#method-EnsembleFSResult-new)

- [`EnsembleFSResult$format()`](#method-EnsembleFSResult-format)

- [`EnsembleFSResult$print()`](#method-EnsembleFSResult-print)

- [`EnsembleFSResult$help()`](#method-EnsembleFSResult-help)

- [`EnsembleFSResult$set_active_measure()`](#method-EnsembleFSResult-set_active_measure)

- [`EnsembleFSResult$combine()`](#method-EnsembleFSResult-combine)

- [`EnsembleFSResult$feature_ranking()`](#method-EnsembleFSResult-feature_ranking)

- [`EnsembleFSResult$stability()`](#method-EnsembleFSResult-stability)

- [`EnsembleFSResult$pareto_front()`](#method-EnsembleFSResult-pareto_front)

- [`EnsembleFSResult$knee_points()`](#method-EnsembleFSResult-knee_points)

- [`EnsembleFSResult$clone()`](#method-EnsembleFSResult-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    EnsembleFSResult$new(
      result,
      features,
      benchmark_result = NULL,
      measure,
      inner_measure = NULL
    )

#### Arguments

- `result`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  The result of the ensemble feature selection. Mandatory column names
  should include `"resampling_iteration"`, `"learner_id"`, `"features"`
  and `"n_features"`. A column named as `{measure$id}` (scores on the
  test sets) must also be always present. The column with the
  performance scores on the inner resampling of the train sets is not
  mandatory, but note that it should be named as
  `{inner_measure$id}_inner` to distinguish from the `{measure$id}`.

- `features`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The vector of features of the task that was used in the ensemble
  feature selection.

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  The benchmark result object.

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  The performance measure used to evaluate the learners on the test sets
  generated during the ensemble feature selection process. By default,
  this serves as the 'active' measure for the methods of this object.
  The active measure can be updated using the `$set_active_measure()`
  method.

- `inner_measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  The performance measure used to optimize and evaluate the learners
  during the inner resampling process of the training sets, generated as
  part of the ensemble feature selection procedure.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    EnsembleFSResult$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    EnsembleFSResult$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    EnsembleFSResult$help()

------------------------------------------------------------------------

### Method `set_active_measure()`

Use this function to change the active measure.

#### Usage

    EnsembleFSResult$set_active_measure(which = "outer")

#### Arguments

- `which`:

  (`character(1)`)  
  Which [measure](https://mlr3.mlr-org.com/reference/Measure.html) from
  the ensemble feature selection result to use in methods of this
  object. Should be either `"inner"` (optimization measure used in
  training sets) or `"outer"` (measure used in test sets, default
  value).

------------------------------------------------------------------------

### Method `combine()`

Combines a second EnsembleFSResult into the current object, modifying it
**in-place**. If the second EnsembleFSResult (`efsr`) is `NULL`, the
method returns the object unmodified.

Both objects must have the same task features and `measure`. If the
`inner_measure` differs between the objects or is `NULL` in either, it
will be set to `NULL` in the combined object. Additionally, the
`importance` column will be removed if it is missing in either object.
If both objects contain a `benchmark_result`, these will be combined.
Otherwise, the combined object will have a `NULL` value for
`benchmark_result`.

This method modifies the object by reference. To preserve the original
state, explicitly `$clone()` the object beforehand. Alternatively, you
can use the [`c()`](https://rdrr.io/r/base/c.html) function, which
internally calls this method.

#### Usage

    EnsembleFSResult$combine(efsr)

#### Arguments

- `efsr`:

  (EnsembleFSResult)  
  A second EnsembleFSResult object to combine with the current object.

#### Returns

Returns the object itself, but modified **by reference**.

------------------------------------------------------------------------

### Method `feature_ranking()`

Calculates the feature ranking via
[`fastVoteR::rank_candidates()`](https://bblodfon.github.io/fastVoteR/reference/rank_candidates.html).

#### Usage

    EnsembleFSResult$feature_ranking(
      method = "av",
      use_weights = TRUE,
      committee_size = NULL,
      shuffle_features = TRUE
    )

#### Arguments

- `method`:

  (`character(1)`)  
  The method to calculate the feature ranking. See
  [`fastVoteR::rank_candidates()`](https://bblodfon.github.io/fastVoteR/reference/rank_candidates.html)
  for a complete list of available methods. Approval voting (`"av"`) is
  the default method.

- `use_weights`:

  (`logical(1)`)  
  The default value (`TRUE`) uses weights equal to the performance
  scores of each voter/model (or the inverse scores if the measure is
  minimized). If `FALSE`, we treat all voters as equal and assign them
  all a weight equal to 1.

- `committee_size`:

  (`integer(1)`)  
  Number of top selected features in the output ranking. This parameter
  can be used to speed-up methods that build a committee sequentially
  (`"seq_pav"`), by requesting only the top N selected
  candidates/features and not the complete feature ranking.

- `shuffle_features`:

  (`logical(1)`)  
  Whether to shuffle the task features randomly before computing the
  ranking. Shuffling ensures consistent random tie-breaking across
  methods and prevents deterministic biases when features with equal
  scores are encountered. Default is `TRUE` and it's advised to set a
  seed before running this function. Set to `FALSE` if deterministic
  ordering of features is preferred (same as during initialization).

#### Details

The feature ranking process is built on the following framework: models
act as *voters*, features act as *candidates*, and voters select certain
candidates (features). The primary objective is to compile these
selections into a consensus ranked list of features, effectively forming
a committee.

For every feature a score is calculated, which depends on the `"method"`
argument. The higher the score, the higher the ranking of the feature.
Note that some methods output a feature ranking instead of a score per
feature, so we always include **Borda's score**, which is
method-agnostic, i.e. it can be used to compare the feature rankings
across different methods.

We shuffle the input candidates/features so that we enforce random
tie-breaking. Users should set the same `seed` for consistent comparison
between the different feature ranking methods and for reproducibility.

#### Returns

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
listing all the features, ordered by decreasing scores (depends on the
`"method"`). Columns are as follows:

- `"feature"`: Feature names.

- `"score"`: Scores assigned to each feature based on the selected
  method (if applicable).

- `"norm_score"`: Normalized scores (if applicable), scaled to the range
  \\\[0,1\]\\, which can be loosely interpreted as **selection
  probabilities** (Meinshausen et al. (2010)).

- `"borda_score"`: Borda scores for method-agnostic comparison, ranging
  in \\\[0,1\]\\, where the top feature receives a score of 1 and the
  lowest-ranked feature receives a score of 0. This column is always
  included so that feature ranking methods that output only rankings
  have also a feature-wise score.

------------------------------------------------------------------------

### Method `stability()`

Calculates the stability of the selected features with the
[stabm](https://CRAN.R-project.org/package=stabm) package. The results
are cached. When the same stability measure is requested again with
different arguments, the cache must be reset.

#### Usage

    EnsembleFSResult$stability(
      stability_measure = "jaccard",
      stability_args = NULL,
      global = TRUE,
      reset_cache = FALSE
    )

#### Arguments

- `stability_measure`:

  (`character(1)`)  
  The stability measure to be used. One of the measures returned by
  [`stabm::listStabilityMeasures()`](https://bommert.github.io/stabm/reference/listStabilityMeasures.html)
  in lower case. Default is `"jaccard"`.

- `stability_args`:

  (`list`)  
  Additional arguments passed to the stability measure function.

- `global`:

  (`logical(1)`)  
  Whether to calculate the stability globally or for each learner.

- `reset_cache`:

  (`logical(1)`)  
  If `TRUE`, the cached results are ignored.

#### Returns

A [`numeric()`](https://rdrr.io/r/base/numeric.html) value representing
the stability of the selected features. Or a
[`numeric()`](https://rdrr.io/r/base/numeric.html) vector with the
stability of the selected features for each learner.

------------------------------------------------------------------------

### Method `pareto_front()`

This function identifies the **Pareto front** of the ensemble feature
selection process, i.e., the set of points that represent the trade-off
between the number of features and performance (e.g. classification
error).

#### Usage

    EnsembleFSResult$pareto_front(type = "empirical", max_nfeatures = NULL)

#### Arguments

- `type`:

  (`character(1)`)  
  Specifies the type of Pareto front to return. See details.

- `max_nfeatures`:

  (`integer(1)`)  
  Specifies the maximum number of features for which the estimated
  Pareto front is computed. Applicable only when `type = "estimated"`.
  If `NULL` (default), the maximum number of features is determined by
  the ensemble feature selection process.

#### Details

Two options are available for the Pareto front:

- `"empirical"` (default): returns the empirical Pareto front.

- `"estimated"`: the Pareto front points are estimated by fitting a
  linear model with the inversed of the number of features (\\1/x\\) as
  input and the associated performance scores as output.

This method is useful when the Pareto points are sparse and the front
assumes a convex shape if better performance corresponds to lower
measure values (e.g. classification error), or a concave shape otherwise
(e.g. classification accuracy).

When `type = "estimated"`, the estimated Pareto front includes points
with the number of features ranging from 1 up to `max_nfeatures`. If
`max_nfeatures` is not provided, it defaults to the maximum number of
features available in the ensemble feature selection `result`, i.e. the
maximum out of all learners and resamplings included.

#### Returns

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns the number of features and the performance that together
form the Pareto front.

------------------------------------------------------------------------

### Method `knee_points()`

This function implements various *knee* point identification (KPI)
methods, which select points in the Pareto front, such that an optimal
trade-off between performance and number of features is achieved. In
most cases, only one such point is returned.

#### Usage

    EnsembleFSResult$knee_points(
      method = "NBI",
      type = "empirical",
      max_nfeatures = NULL
    )

#### Arguments

- `method`:

  (`character(1)`)  
  Type of method to use to identify the knee point.

- `type`:

  (`character(1)`)  
  Specifies the type of Pareto front to use for the identification of
  the knee point.

- `max_nfeatures`:

  (`integer(1)`)  
  Specifies the maximum number of features for which the estimated
  Pareto front is computed. Applicable only when `type = "estimated"`.
  If `NULL` (default), the maximum number of features is determined by
  the ensemble feature selection process. See `pareto_front()` method
  for more details.

#### Details

The available KPI methods are:

- `"NBI"` (default): The **Normal-Boundary Intersection** method is a
  geometry-based method which calculates the perpendicular distance of
  each point from the line connecting the first and last points of the
  Pareto front. The knee point is determined as the Pareto point with
  the maximum distance from this line, see Das (1999).

#### Returns

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with the knee point(s) of the Pareto front.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EnsembleFSResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
  efsr = ensemble_fselect(
    fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    inner_measure = msr("classif.ce"),
    measure = msr("classif.acc"),
    terminator = trm("none")
  )

  # contains the benchmark result
  efsr$benchmark_result
#> 
#> ── <BenchmarkResult> of 4 rows with 2 resampling run ───────────────────────────
#>  nr task_id                    learner_id resampling_id iters warnings errors
#>   1   sonar       classif.rpart.fselector   subsampling     2        0      0
#>   2   sonar classif.featureless.fselector   subsampling     2        0      0

  # contains the selected features for each iteration
  efsr$result
#>                       learner_id resampling_iteration classif.acc
#>                           <char>                <int>       <num>
#> 1:       classif.rpart.fselector                    1   0.7101449
#> 2:       classif.rpart.fselector                    2   0.7391304
#> 3: classif.featureless.fselector                    1   0.4782609
#> 4: classif.featureless.fselector                    2   0.5072464
#>                           features n_features classif.ce_inner
#>                             <list>      <int>            <num>
#> 1: V10,V11,V12,V13,V14,V15,...[19]         19        0.1802344
#> 2: V10,V11,V12,V13,V15,V16,...[10]         10        0.2958680
#> 3:                          V22,V8          2        0.4383287
#> 4:                         V21,V25          2        0.4529756
#>                                                             importance
#>                                                                 <list>
#> 1:       17.33333,16.33333,15.00000,14.00000,12.00000,10.66667,...[19]
#> 2: 10.000000, 8.333333, 7.333333, 6.666667, 5.666667, 5.000000,...[10]
#> 3:                                                   1.666667,1.333333
#> 4:                                                                 2,1
#>                   task                                       learner
#>                 <list>                                        <list>
#> 1: <TaskClassif:sonar>       <AutoFSelector:classif.rpart.fselector>
#> 2: <TaskClassif:sonar>       <AutoFSelector:classif.rpart.fselector>
#> 3: <TaskClassif:sonar> <AutoFSelector:classif.featureless.fselector>
#> 4: <TaskClassif:sonar> <AutoFSelector:classif.featureless.fselector>
#>                 resampling
#>                     <list>
#> 1: <ResamplingSubsampling>
#> 2: <ResamplingSubsampling>
#> 3: <ResamplingSubsampling>
#> 4: <ResamplingSubsampling>

  # returns the stability of the selected features
  efsr$stability(stability_measure = "jaccard")
#> [1] 0.1008772

  # returns a ranking of all features
  head(efsr$feature_ranking())
#>    feature    score norm_score borda_score
#>     <char>    <num>      <num>       <num>
#> 1:     V15 1.449275  0.5952381   1.0000000
#> 2:     V10 1.449275  0.5952381   0.9830508
#> 3:     V16 1.449275  0.5952381   0.9661017
#> 4:     V36 1.449275  0.5952381   0.9491525
#> 5:     V17 1.449275  0.5952381   0.9322034
#> 6:     V11 1.449275  0.5952381   0.9152542

  # returns the empirical pareto front, i.e. n_features vs measure (error)
  efsr$pareto_front()
#>    n_features classif.acc
#>         <num>       <num>
#> 1:          2   0.4782609
#> 2:          2   0.5072464
#> 3:         10   0.7391304

  # returns the knee points (optimal trade-off between n_features and performance)
  efsr$knee_points()
#>    n_features classif.acc
#>         <num>       <num>
#> 1:          2   0.5072464

  # change to use the inner optimization measure
  efsr$set_active_measure(which = "inner")

  # Pareto front is calculated on the inner measure
  efsr$pareto_front()
#>    n_features classif.ce_inner
#>         <num>            <num>
#> 1:          2        0.4529756
#> 2:          2        0.4383287
#> 3:         10        0.2958680
#> 4:         19        0.1802344
# }
```
