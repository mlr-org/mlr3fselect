# Feature Selection with Genetic Search

Feature selection using the Genetic Algorithm from the package
[genalg](https://CRAN.R-project.org/package=genalg).

## Dictionary

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("genetic_search")

## Control Parameters

For the meaning of the control parameters, see
[`genalg::rbga.bin()`](https://rdrr.io/pkg/genalg/man/rbga.bin.html).
[`genalg::rbga.bin()`](https://rdrr.io/pkg/genalg/man/rbga.bin.html)
internally terminates after `iters` iteration. We set `ìters = 100000`
to allow the termination via our terminators. If more iterations are
needed, set `ìters` to a higher value in the parameter set.

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.md)
-\> `FSelectorBatchGeneticSearch`

## Methods

### Public methods

- [`FSelectorBatchGeneticSearch$new()`](#method-FSelectorBatchGeneticSearch-new)

- [`FSelectorBatchGeneticSearch$clone()`](#method-FSelectorBatchGeneticSearch-clone)

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

    FSelectorBatchGeneticSearch$new()

------------------------------------------------------------------------

### Method `clone()`

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
#> 1:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>          features n_features classif.ce
#>            <list>      <int>      <num>
#> 1: flipper_length          1  0.1478261

# all evaluated feature sets
as.data.table(instance$archive)
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>  1:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  2:      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE  FALSE
#>  3:      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE  FALSE
#>  4:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  5:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#>  6:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  7:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE   TRUE
#>  8:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#>  9:      FALSE       FALSE     FALSE          FALSE  FALSE   TRUE  FALSE
#> 10:      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE  FALSE
#>     classif.ce runtime_learners           timestamp batch_nr warnings errors
#>          <num>            <num>              <POSc>    <int>    <int>  <int>
#>  1:  0.1478261            0.004 2025-12-13 15:56:45        1        0      0
#>  2:  0.2521739            0.022 2025-12-13 15:56:45        2        0      0
#>  3:  0.2521739            0.004 2025-12-13 15:56:45        3        0      0
#>  4:  0.1478261            0.005 2025-12-13 15:56:45        4        0      0
#>  5:  0.2521739            0.003 2025-12-13 15:56:46        5        0      0
#>  6:  0.5826087            0.004 2025-12-13 15:56:46        6        0      0
#>  7:  0.5826087            0.004 2025-12-13 15:56:46        7        0      0
#>  8:  0.2521739            0.004 2025-12-13 15:56:46        8        0      0
#>  9:  0.5826087            0.004 2025-12-13 15:56:46        9        0      0
#> 10:  0.2521739            0.003 2025-12-13 15:56:46       10        0      0
#>           features n_features  resample_result
#>             <list>     <list>           <list>
#>  1: flipper_length          1 <ResampleResult>
#>  2:         island          1 <ResampleResult>
#>  3:         island          1 <ResampleResult>
#>  4: flipper_length          1 <ResampleResult>
#>  5:      body_mass          1 <ResampleResult>
#>  6:           year          1 <ResampleResult>
#>  7:           year          1 <ResampleResult>
#>  8:      body_mass          1 <ResampleResult>
#>  9:            sex          1 <ResampleResult>
#> 10:      body_mass          1 <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
