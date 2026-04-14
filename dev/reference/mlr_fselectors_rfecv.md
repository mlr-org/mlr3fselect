# Feature Selection with Recursive Feature Elimination with Cross Validation

Feature selection using the Recursive Feature Elimination with
Cross-Validation (RFE-CV) algorithm. See
[FSelectorBatchRFE](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfe.md)
for a description of the base algorithm. RFE-CV runs a recursive feature
elimination in each iteration of a cross-validation to determine the
optimal number of features. Then a recursive feature elimination is run
again on the complete dataset with the optimal number of features as the
final feature set size. The performance of the optimal feature set is
calculated on the complete data set and should not be reported as the
performance of the final model. Only works with
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)s that
can calculate importance scores (see the section on optional extractors
in [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)).

## Details

The resampling strategy is changed during the feature selection. The
resampling strategy passed to the instance (`resampling`) is used to
determine the optimal number of features. Usually, a cross-validation
strategy is used and a recursive feature elimination is run in each
iteration of the cross-validation. Internally,
[mlr3::ResamplingCustom](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom.html)
is used to emulate this part of the algorithm. In the final recursive
feature elimination run the resampling strategy is changed to
[mlr3::ResamplingInsample](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.html)
i.e. the complete data set is used for training and testing.

The feature selection terminates itself when the optimal number of
features is reached. It is not necessary to set a termination criterion.

## Archive

The
[ArchiveBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveBatchFSelect.md)
holds the following additional columns:

- `"iteration"` (`integer(1)`)  
  The resampling iteration in which the feature subset was evaluated.

- `"importance"` ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  The importance score vector of the feature subset.

## Resources

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

- Utilize the built-in feature importance of models with [Recursive
  Feature
  Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).

## Dictionary

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

    fs("rfe")

## Control Parameters

- `n_features`:

  `integer(1)`  
  The number of features to select. By default half of the features are
  selected.

- `feature_fraction`:

  `double(1)`  
  Fraction of features to retain in each iteration. The default 0.5
  retrains half of the features.

- `feature_number`:

  `integer(1)`  
  Number of features to remove in each iteration.

- `subset_sizes`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Vector of number of features to retain in each iteration. Must be
  sorted in decreasing order.

- `recursive`:

  `logical(1)`  
  If `TRUE` (default), the feature importance is calculated in each
  iteration.

The parameter `feature_fraction`, `feature_number` and `subset_sizes`
are mutually exclusive.

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchRFECV`

## Methods

### Public methods

- [`FSelectorBatchRFECV$new()`](#method-FSelectorBatchRFECV-new)

- [`FSelectorBatchRFECV$clone()`](#method-FSelectorBatchRFECV-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatchRFECV$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchRFECV$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Feature Selection
# \donttest{

# retrieve task and load learner
task = tsk("penguins")
learner = lrn("classif.rpart")

# run feature selection on the Palmer Penguins data set
instance = fselect(
  fselector = fs("rfecv"),
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  store_models = TRUE
)

# best performing feature subset
instance$result
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>                                 features n_features classif.ce
#>                                   <list>      <int>      <num>
#> 1: bill_depth,bill_length,flipper_length          3  0.0377907

# all evaluated feature subsets
as.data.table(instance$archive)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#> 2:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#> 3:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#> 4:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 5:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 6:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 7:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#> 8:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>    classif.ce runtime_learners           timestamp batch_nr warnings errors
#>         <num>            <num>              <POSc>    <int>    <int>  <int>
#> 1: 0.08695652            0.007 2026-04-14 16:24:14        1        0      0
#> 2: 0.04347826            0.006 2026-04-14 16:24:14        1        0      0
#> 3: 0.07017544            0.006 2026-04-14 16:24:14        1        0      0
#> 4: 0.08695652            0.006 2026-04-14 16:24:14        2        0      0
#> 5: 0.05217391            0.005 2026-04-14 16:24:14        2        0      0
#> 6: 0.07017544            0.005 2026-04-14 16:24:14        2        0      0
#> 7: 0.03488372            0.006 2026-04-14 16:24:14        3        0      0
#> 8: 0.03779070            0.007 2026-04-14 16:24:14        4        0      0
#>                                                            importance iteration
#>                                                                <list>     <int>
#> 1:       91.03050,79.32583,65.55837,60.87563,49.73684, 0.00000,...[7]         1
#> 2:       93.12860,78.51597,66.59879,57.66769,54.71082, 0.00000,...[7]         2
#> 3:       85.98832,78.55965,67.19489,61.23953,43.49718, 0.00000,...[7]         3
#> 4:                                         91.03050,79.32583,65.55837         1
#> 5:                                         93.12860,78.51597,66.59879         2
#> 6:                                         85.98832,78.55965,67.19489         3
#> 7: 124.20793,121.52400,102.74919, 87.26186, 78.61700,  0.00000,...[7]        NA
#> 8:                                         124.2079,121.5240,104.2507        NA
#>                                                             features n_features
#>                                                               <list>     <list>
#> 1: bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]          7
#> 2: bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]          7
#> 3: bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]          7
#> 4:                             bill_depth,bill_length,flipper_length          3
#> 5:                             bill_depth,bill_length,flipper_length          3
#> 6:                             bill_depth,bill_length,flipper_length          3
#> 7: bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]          7
#> 8:                             bill_depth,bill_length,flipper_length          3
#>     resample_result
#>              <list>
#> 1: <ResampleResult>
#> 2: <ResampleResult>
#> 3: <ResampleResult>
#> 4: <ResampleResult>
#> 5: <ResampleResult>
#> 6: <ResampleResult>
#> 7: <ResampleResult>
#> 8: <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
