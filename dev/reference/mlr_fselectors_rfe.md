# Feature Selection with Recursive Feature Elimination

Feature selection using the Recursive Feature Elimination (RFE)
algorithm. Recursive feature elimination iteratively removes features
with a low importance score. Only works with
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)s that
can calculate importance scores (see the section on optional extractors
in [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)).

## Source

Guyon I, Weston J, Barnhill S, Vapnik V (2002). “Gene Selection for
Cancer Classification using Support Vector Machines.” *Machine
Learning*, **46**(1), 389–422. ISSN 1573-0565,
[doi:10.1023/A:1012487302797](https://doi.org/10.1023/A%3A1012487302797)
.

## Details

The learner is trained on all features at the start and importance
scores are calculated for each feature. Then the least important feature
is removed and the learner is trained on the reduced feature set. The
importance scores are calculated again and the procedure is repeated
until the desired number of features is reached. The non-recursive
option (`recursive = FALSE`) only uses the importance scores calculated
in the first iteration.

The feature selection terminates itself when `n_features` is reached. It
is not necessary to set a termination criterion.

When using a cross-validation resampling strategy, the importance scores
of the resampling iterations are aggregated. The parameter `aggregation`
determines how the importance scores are aggregated. By default
(`"rank"`), the importance score vector of each fold is ranked and the
feature with the lowest average rank is removed. The option `"mean"`
averages the score of each feature across the resampling iterations and
removes the feature with the lowest average score. Averaging the scores
is not appropriate for most importance measures.

## Archive

The
[ArchiveBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveBatchFSelect.md)
holds the following additional columns:

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
  The minimum number of features to select, by default half of the
  features.

- `feature_fraction`:

  `double(1)`  
  Fraction of features to retain in each iteration. The default of 0.5
  retains half of the features.

- `feature_number`:

  `integer(1)`  
  Number of features to remove in each iteration.

- `subset_sizes`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Vector of the number of features to retain in each iteration. Must be
  sorted in decreasing order.

- `recursive`:

  `logical(1)`  
  If `TRUE` (default), the feature importance is calculated in each
  iteration.

- `aggregation`:

  `character(1)`  
  The aggregation method for the importance scores of the resampling
  iterations. See details.

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
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchRFE`

## Methods

### Public methods

- [`FSelectorBatchRFE$new()`](#method-FSelectorBatchRFE-new)

- [`FSelectorBatchRFE$clone()`](#method-FSelectorBatchRFE-clone)

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

    FSelectorBatchRFE$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchRFE$clone(deep = FALSE)

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
  fselector = fs("rfe"),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  store_models = TRUE
)

# best performing feature subset
instance$result
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>            importance
#>                <list>
#> 1: 7,6,5,4,3,2,...[7]
#>                                                             features n_features
#>                                                               <list>      <int>
#> 1: bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]          7
#>    classif.ce
#>         <num>
#> 1: 0.07826087

# all evaluated feature subsets
as.data.table(instance$archive)
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#> 2:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>    classif.ce runtime_learners           timestamp batch_nr warnings errors
#>         <num>            <num>              <POSc>    <int>    <int>  <int>
#> 1: 0.07826087            0.025 2026-03-18 16:09:22        1        0      0
#> 2: 0.08695652            0.006 2026-03-18 16:09:22        2        0      0
#>            importance
#>                <list>
#> 1: 7,6,5,4,3,2,...[7]
#> 2:              3,2,1
#>                                                             features n_features
#>                                                               <list>     <list>
#> 1: bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]          7
#> 2:                             bill_depth,bill_length,flipper_length          3
#>     resample_result
#>              <list>
#> 1: <ResampleResult>
#> 2: <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
