# Feature Selection with Random Search

Feature selection using Random Search Algorithm.

## Source

Bergstra J, Bengio Y (2012). “Random Search for Hyper-Parameter
Optimization.” *Journal of Machine Learning Research*, **13**(10),
281–305. <https://jmlr.csail.mit.edu/papers/v13/bergstra12a.html>.

## Details

The feature sets are randomly drawn. The sets are evaluated in batches
of size `batch_size`. Larger batches mean we can parallelize more,
smaller batches imply a more fine-grained checking of termination
criteria.

## Dictionary

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("random_search")

## Control Parameters

- `max_features`:

  `integer(1)`  
  Maximum number of features. By default, number of features in
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

- `batch_size`:

  `integer(1)`  
  Maximum number of feature sets to try in a batch.

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.md)
-\> `FSelectorBatchRandomSearch`

## Methods

### Public methods

- [`FSelectorBatchRandomSearch$new()`](#method-FSelectorBatchRandomSearch-new)

- [`FSelectorBatchRandomSearch$clone()`](#method-FSelectorBatchRandomSearch-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatchRandomSearch$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchRandomSearch$clone(deep = FALSE)

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
  fselector = fs("random_search"),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)

# best performing feature subset
instance$result
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE      TRUE          FALSE  FALSE  FALSE   TRUE
#>                                 features n_features classif.ce
#>                                   <list>      <int>      <num>
#> 1: bill_depth,bill_length,body_mass,year          4 0.06086957

# all evaluated feature subsets
as.data.table(instance$archive)
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  2:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE   TRUE
#>  3:       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE  FALSE
#>  4:       TRUE        TRUE      TRUE           TRUE   TRUE  FALSE   TRUE
#>  5:      FALSE       FALSE      TRUE          FALSE   TRUE  FALSE  FALSE
#>  6:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  7:       TRUE        TRUE      TRUE          FALSE  FALSE  FALSE   TRUE
#>  8:       TRUE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  9:      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 10:       TRUE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#>     classif.ce runtime_learners           timestamp batch_nr warnings errors
#>          <num>            <num>              <POSc>    <int>    <int>  <int>
#>  1: 0.21739130            0.005 2025-11-27 11:01:27        1        0      0
#>  2: 0.08695652            0.006 2025-11-27 11:01:27        1        0      0
#>  3: 0.08695652            0.005 2025-11-27 11:01:27        1        0      0
#>  4: 0.08695652            0.005 2025-11-27 11:01:27        1        0      0
#>  5: 0.30434783            0.005 2025-11-27 11:01:27        1        0      0
#>  6: 0.07826087            0.005 2025-11-27 11:01:27        1        0      0
#>  7: 0.06086957            0.005 2025-11-27 11:01:27        1        0      0
#>  8: 0.20000000            0.005 2025-11-27 11:01:27        1        0      0
#>  9: 0.36521739            0.004 2025-11-27 11:01:27        1        0      0
#> 10: 0.20000000            0.005 2025-11-27 11:01:27        1        0      0
#>                                                           features n_features
#>                                                             <list>     <list>
#>  1:                                               bill_length,year          2
#>  2: bill_depth,bill_length,body_mass,flipper_length,island,sex,...          7
#>  3:     bill_depth,bill_length,body_mass,flipper_length,island,sex          6
#>  4:    bill_depth,bill_length,body_mass,flipper_length,island,year          6
#>  5:                                               body_mass,island          2
#>  6:                                     bill_length,flipper_length          2
#>  7:                          bill_depth,bill_length,body_mass,year          4
#>  8:                                                     bill_depth          1
#>  9:                                                         island          1
#> 10:                                                     bill_depth          1
#>      resample_result
#>               <list>
#>  1: <ResampleResult>
#>  2: <ResampleResult>
#>  3: <ResampleResult>
#>  4: <ResampleResult>
#>  5: <ResampleResult>
#>  6: <ResampleResult>
#>  7: <ResampleResult>
#>  8: <ResampleResult>
#>  9: <ResampleResult>
#> 10: <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
