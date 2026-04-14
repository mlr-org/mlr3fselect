# Feature Selection with Design Points

Feature selection using user-defined feature sets.

## Details

The feature sets are evaluated in order as given.

The feature selection terminates itself when all feature sets are
evaluated. It is not necessary to set a termination criterion.

## Dictionary

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

    fs("design_points")

## Parameters

- `batch_size`:

  `integer(1)`  
  Maximum number of configurations to try in a batch.

- `design`:

  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Design points to try in search, one per row.

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_exhaustive_search.md),
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
-\>
[`mlr3fselect::FSelectorBatchFromOptimizerBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatchFromOptimizerBatch.md)
-\> `FSelectorBatchDesignPoints`

## Methods

### Public methods

- [`FSelectorBatchDesignPoints$new()`](#method-FSelectorBatchDesignPoints-new)

- [`FSelectorBatchDesignPoints$clone()`](#method-FSelectorBatchDesignPoints-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorBatchFromOptimizerBatch$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatchFromOptimizerBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatchDesignPoints$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchDesignPoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Feature Selection
# \donttest{

# retrieve task and load learner
task = tsk("pima")
learner = lrn("classif.rpart")

# create design
design = mlr3misc::rowwise_table(
  ~age, ~glucose, ~insulin, ~mass, ~pedigree, ~pregnant, ~pressure, ~triceps,
  TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       FALSE,    TRUE,
  TRUE, TRUE,     FALSE,    TRUE,  FALSE,     TRUE,       FALSE,    FALSE,
  TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       FALSE,    FALSE,
  TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       TRUE,     TRUE
)

# run feature selection on the Pima Indians diabetes data set
instance = fselect(
  fselector = fs("design_points", design = design),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce")
)

# best performing feature set
instance$result
#>       age glucose insulin   mass pedigree pregnant pressure triceps
#>    <lgcl>  <lgcl>  <lgcl> <lgcl>   <lgcl>   <lgcl>   <lgcl>  <lgcl>
#> 1:   TRUE    TRUE   FALSE   TRUE    FALSE     TRUE    FALSE   FALSE
#>                     features n_features classif.ce
#>                       <list>      <int>      <num>
#> 1: age,glucose,mass,pregnant          4  0.2578125

# all evaluated feature sets
as.data.table(instance$archive)
#>       age glucose insulin   mass pedigree pregnant pressure triceps classif.ce
#>    <lgcl>  <lgcl>  <lgcl> <lgcl>   <lgcl>   <lgcl>   <lgcl>  <lgcl>      <num>
#> 1:   TRUE   FALSE    TRUE   TRUE    FALSE     TRUE    FALSE    TRUE  0.3085938
#> 2:   TRUE    TRUE   FALSE   TRUE    FALSE     TRUE    FALSE   FALSE  0.2578125
#> 3:   TRUE   FALSE    TRUE   TRUE    FALSE     TRUE    FALSE   FALSE  0.2929688
#> 4:   TRUE   FALSE    TRUE   TRUE    FALSE     TRUE     TRUE    TRUE  0.3085938
#>    runtime_learners           timestamp batch_nr warnings errors
#>               <num>              <POSc>    <int>    <int>  <int>
#> 1:            0.009 2026-04-14 12:31:32        1        0      0
#> 2:            0.009 2026-04-14 12:31:32        2        0      0
#> 3:            0.007 2026-04-14 12:31:32        3        0      0
#> 4:            0.009 2026-04-14 12:31:32        4        0      0
#>                                      features n_features  resample_result
#>                                        <list>     <list>           <list>
#> 1:          age,insulin,mass,pregnant,triceps          5 <ResampleResult>
#> 2:                  age,glucose,mass,pregnant          4 <ResampleResult>
#> 3:                  age,insulin,mass,pregnant          4 <ResampleResult>
#> 4: age,insulin,mass,pregnant,pressure,triceps          6 <ResampleResult>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
