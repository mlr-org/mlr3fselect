# Feature Selection with Exhaustive Search

Feature Selection using the Exhaustive Search Algorithm. Exhaustive
Search generates all possible feature sets.

## Details

The feature selection terminates itself when all feature sets are
evaluated. It is not necessary to set a termination criterion.

## Dictionary

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

    fs("exhaustive_search")

## Control Parameters

- `max_features`:

  `integer(1)`  
  Maximum number of features. By default, number of features in
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchExhaustiveSearch`

## Methods

### Public methods

- [`FSelectorBatchExhaustiveSearch$new()`](#method-FSelectorBatchExhaustiveSearch-new)

- [`FSelectorBatchExhaustiveSearch$clone()`](#method-FSelectorBatchExhaustiveSearch-clone)

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

    FSelectorBatchExhaustiveSearch$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchExhaustiveSearch$clone(deep = FALSE)

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
  fselector = fs("exhaustive_search"),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)

# best performing feature set
instance$result
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:       TRUE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#>                  features n_features classif.ce
#>                    <list>      <int>      <num>
#> 1: bill_depth,bill_length          2 0.07826087

# all evaluated feature sets
as.data.table(instance$archive)
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:       TRUE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  2:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  3:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#>  4:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  5:      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE  FALSE
#>  6:      FALSE       FALSE     FALSE          FALSE  FALSE   TRUE  FALSE
#>  7:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  8:       TRUE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  9:       TRUE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#> 10:       TRUE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>     classif.ce runtime_learners           timestamp batch_nr warnings errors
#>          <num>            <num>              <POSc>    <int>    <int>  <int>
#>  1: 0.20000000            0.007 2026-04-10 10:37:03        1        0      0
#>  2: 0.26956522            0.005 2026-04-10 10:37:03        1        0      0
#>  3: 0.20000000            0.005 2026-04-10 10:37:03        1        0      0
#>  4: 0.18260870            0.005 2026-04-10 10:37:03        1        0      0
#>  5: 0.25217391            0.007 2026-04-10 10:37:03        1        0      0
#>  6: 0.60000000            0.006 2026-04-10 10:37:03        1        0      0
#>  7: 0.60000000            0.004 2026-04-10 10:37:03        1        0      0
#>  8: 0.07826087            0.005 2026-04-10 10:37:03        1        0      0
#>  9: 0.18260870            0.005 2026-04-10 10:37:03        1        0      0
#> 10: 0.22608696            0.004 2026-04-10 10:37:03        1        0      0
#>                      features n_features  resample_result
#>                        <list>     <list>           <list>
#>  1:                bill_depth          1 <ResampleResult>
#>  2:               bill_length          1 <ResampleResult>
#>  3:                 body_mass          1 <ResampleResult>
#>  4:            flipper_length          1 <ResampleResult>
#>  5:                    island          1 <ResampleResult>
#>  6:                       sex          1 <ResampleResult>
#>  7:                      year          1 <ResampleResult>
#>  8:    bill_depth,bill_length          2 <ResampleResult>
#>  9:      bill_depth,body_mass          2 <ResampleResult>
#> 10: bill_depth,flipper_length          2 <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
