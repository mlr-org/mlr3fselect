# Feature Selection with Genetic Search

Feature selection using the Genetic Algorithm from the package
[genalg](https://CRAN.R-project.org/package=genalg).

## Dictionary

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

    fs("genetic_search")

## Control Parameters

For the meaning of the control parameters, see
[`genalg::rbga.bin()`](https://rdrr.io/pkg/genalg/man/rbga.bin.html).
[`genalg::rbga.bin()`](https://rdrr.io/pkg/genalg/man/rbga.bin.html)
internally terminates after `iters` iteration. We set `iters = 100000`
to allow the termination via our terminators. If more iterations are
needed, set `iters` to a higher value in the parameter set.

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchGeneticSearch`

## Methods

### Public methods

- [`FSelectorBatchGeneticSearch$new()`](#method-FSelectorBatchGeneticSearch-initialize)

- [`FSelectorBatchGeneticSearch$clone()`](#method-FSelectorBatchGeneticSearch-clone)

Inherited methods

- [`FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.html#method-optimize)

------------------------------------------------------------------------

### `FSelectorBatchGeneticSearch$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatchGeneticSearch$new()

------------------------------------------------------------------------

### `FSelectorBatchGeneticSearch$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchGeneticSearch$clone(deep = FALSE)

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
  fselector = fs("genetic_search"),
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
#> 1:      FALSE       FALSE     FALSE           TRUE   TRUE  FALSE   TRUE
#>                      features n_features classif.ce
#>                        <list>      <int>      <num>
#> 1: flipper_length,island,year          3  0.1565217

# all evaluated feature sets
as.data.table(instance$archive)
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE  FALSE
#>  2:       TRUE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  3:      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE  FALSE
#>  4:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  5:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  6:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  7:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  8:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  9:      FALSE       FALSE     FALSE           TRUE   TRUE  FALSE   TRUE
#> 10:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>     classif.ce runtime_learners           timestamp batch_nr warnings errors
#>          <num>            <num>              <POSc>    <int>    <int>  <int>
#>  1:  0.2869565            0.006 2026-07-27 10:28:06        1        0      0
#>  2:  0.2782609            0.005 2026-07-27 10:28:06        2        0      0
#>  3:  0.2869565            0.006 2026-07-27 10:28:07        3        0      0
#>  4:  0.5565217            0.005 2026-07-27 10:28:07        4        0      0
#>  5:  0.2173913            0.006 2026-07-27 10:28:07        5        0      0
#>  6:  0.2347826            0.005 2026-07-27 10:28:07        6        0      0
#>  7:  0.5565217            0.005 2026-07-27 10:28:07        7        0      0
#>  8:  0.2347826            0.005 2026-07-27 10:28:07        8        0      0
#>  9:  0.1565217            0.006 2026-07-27 10:28:07        9        0      0
#> 10:  0.5565217            0.005 2026-07-27 10:28:07       10        0      0
#>                       features n_features  resample_result
#>                         <list>     <list>           <list>
#>  1:                     island          1 <ResampleResult>
#>  2:                 bill_depth          1 <ResampleResult>
#>  3:                     island          1 <ResampleResult>
#>  4:                       year          1 <ResampleResult>
#>  5:             flipper_length          1 <ResampleResult>
#>  6:                bill_length          1 <ResampleResult>
#>  7:                       year          1 <ResampleResult>
#>  8:                bill_length          1 <ResampleResult>
#>  9: flipper_length,island,year          3 <ResampleResult>
#> 10:                       year          1 <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
