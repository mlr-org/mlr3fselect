# Feature Selection with Sequential Search

Feature selection using Sequential Search Algorithm.

## Details

Sequential forward selection (`strategy = fsf`) extends the feature set
in each iteration with the feature that increases the model's
performance the most. Sequential backward selection (`strategy = fsb`)
follows the same idea but starts with all features and removes features
from the set.

The feature selection terminates itself when `min_features` or
`max_features` is reached. It is not necessary to set a termination
criterion.

## Dictionary

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("sequential")

## Control Parameters

- `min_features`:

  `integer(1)`  
  Minimum number of features. By default, 1.

- `max_features`:

  `integer(1)`  
  Maximum number of features. By default, number of features in
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

- `strategy`:

  `character(1)`  
  Search method `sfs` (forward search) or `sbs` (backward search).

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_shadow_variable_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.md)
-\> `FSelectorBatchSequential`

## Methods

### Public methods

- [`FSelectorBatchSequential$new()`](#method-FSelectorBatchSequential-new)

- [`FSelectorBatchSequential$optimization_path()`](#method-FSelectorBatchSequential-optimization_path)

- [`FSelectorBatchSequential$clone()`](#method-FSelectorBatchSequential-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.\`

#### Usage

    FSelectorBatchSequential$new()

------------------------------------------------------------------------

### Method `optimization_path()`

Returns the optimization path.

#### Usage

    FSelectorBatchSequential$optimization_path(inst, include_uhash = FALSE)

#### Arguments

- `inst`:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md))  
  Instance optimized with FSelectorBatchSequential.

- `include_uhash`:

  (`logical(1)`)  
  Include `uhash` column?

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchSequential$clone(deep = FALSE)

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
  fselector = fs("sequential"),
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
#> 1:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>                      features n_features classif.ce
#>                        <list>      <int>      <num>
#> 1: bill_length,flipper_length          2 0.06956522

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
#>  8:       TRUE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#>  9:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 10:      FALSE       FALSE      TRUE           TRUE  FALSE  FALSE  FALSE
#> 11:      FALSE       FALSE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 12:      FALSE       FALSE     FALSE           TRUE  FALSE   TRUE  FALSE
#> 13:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE   TRUE
#>     classif.ce runtime_learners           timestamp batch_nr warnings errors
#>          <num>            <num>              <POSc>    <int>    <int>  <int>
#>  1: 0.20869565            0.005 2025-11-27 11:01:30        1        0      0
#>  2: 0.24347826            0.003 2025-11-27 11:01:30        1        0      0
#>  3: 0.25217391            0.004 2025-11-27 11:01:30        1        0      0
#>  4: 0.20000000            0.003 2025-11-27 11:01:30        1        0      0
#>  5: 0.26086957            0.004 2025-11-27 11:01:30        1        0      0
#>  6: 0.59130435            0.004 2025-11-27 11:01:30        1        0      0
#>  7: 0.59130435            0.005 2025-11-27 11:01:30        1        0      0
#>  8: 0.18260870            0.005 2025-11-27 11:01:30        2        0      0
#>  9: 0.06956522            0.005 2025-11-27 11:01:30        2        0      0
#> 10: 0.20000000            0.005 2025-11-27 11:01:30        2        0      0
#> 11: 0.15652174            0.005 2025-11-27 11:01:30        2        0      0
#> 12: 0.20000000            0.005 2025-11-27 11:01:30        2        0      0
#> 13: 0.20000000            0.005 2025-11-27 11:01:30        2        0      0
#>                       features n_features  resample_result
#>                         <list>     <list>           <list>
#>  1:                 bill_depth          1 <ResampleResult>
#>  2:                bill_length          1 <ResampleResult>
#>  3:                  body_mass          1 <ResampleResult>
#>  4:             flipper_length          1 <ResampleResult>
#>  5:                     island          1 <ResampleResult>
#>  6:                        sex          1 <ResampleResult>
#>  7:                       year          1 <ResampleResult>
#>  8:  bill_depth,flipper_length          2 <ResampleResult>
#>  9: bill_length,flipper_length          2 <ResampleResult>
#> 10:   body_mass,flipper_length          2 <ResampleResult>
#> 11:      flipper_length,island          2 <ResampleResult>
#> 12:         flipper_length,sex          2 <ResampleResult>
#> 13:        flipper_length,year          2 <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
