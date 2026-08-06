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

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

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
[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchRandomSearch`

## Methods

### Public methods

- [`FSelectorBatchRandomSearch$new()`](#method-FSelectorBatchRandomSearch-initialize)

- [`FSelectorBatchRandomSearch$clone()`](#method-FSelectorBatchRandomSearch-clone)

Inherited methods

- [`FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.html#method-optimize)

------------------------------------------------------------------------

### `FSelectorBatchRandomSearch$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatchRandomSearch$new()

------------------------------------------------------------------------

### `FSelectorBatchRandomSearch$clone()`

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
#> 1:       TRUE        TRUE     FALSE          FALSE  FALSE   TRUE  FALSE
#>                      features n_features classif.ce
#>                        <list>      <int>      <num>
#> 1: bill_depth,bill_length,sex          3 0.05217391

# all evaluated feature subsets
as.data.table(instance$archive)
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:      FALSE        TRUE      TRUE           TRUE  FALSE   TRUE  FALSE
#>  2:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE   TRUE
#>  3:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  4:       TRUE        TRUE     FALSE          FALSE  FALSE   TRUE  FALSE
#>  5:       TRUE        TRUE     FALSE           TRUE   TRUE   TRUE   TRUE
#>  6:      FALSE        TRUE     FALSE           TRUE  FALSE   TRUE   TRUE
#>  7:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE   TRUE
#>  8:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#>  9:       TRUE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 10:      FALSE        TRUE     FALSE          FALSE  FALSE   TRUE   TRUE
#>     classif.ce runtime_learners           timestamp batch_nr warnings errors
#>          <num>            <num>              <POSc>    <int>    <int>  <int>
#>  1: 0.08695652            0.006 2026-08-06 08:41:23        1        0      0
#>  2: 0.08695652            0.005 2026-08-06 08:41:23        1        0      0
#>  3: 0.61739130            0.004 2026-08-06 08:41:23        1        0      0
#>  4: 0.05217391            0.007 2026-08-06 08:41:23        1        0      0
#>  5: 0.05217391            0.006 2026-08-06 08:41:23        1        0      0
#>  6: 0.08695652            0.006 2026-08-06 08:41:23        1        0      0
#>  7: 0.08695652            0.005 2026-08-06 08:41:23        1        0      0
#>  8: 0.21739130            0.005 2026-08-06 08:41:23        1        0      0
#>  9: 0.18260870            0.005 2026-08-06 08:41:23        1        0      0
#> 10: 0.24347826            0.006 2026-08-06 08:41:23        1        0      0
#>                                                  features n_features
#>                                                    <list>      <int>
#>  1:              bill_length,body_mass,flipper_length,sex          4
#>  2:                       bill_length,flipper_length,year          3
#>  3:                                                  year          1
#>  4:                            bill_depth,bill_length,sex          3
#>  5: bill_depth,bill_length,flipper_length,island,sex,year          6
#>  6:                   bill_length,flipper_length,sex,year          4
#>  7:                       bill_length,flipper_length,year          3
#>  8:                                             body_mass          1
#>  9:                             bill_depth,flipper_length          2
#> 10:                                  bill_length,sex,year          3
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
