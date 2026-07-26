# Feature Selection with Shadow Variable Search

Feature selection using the Shadow Variable Search Algorithm. Shadow
variable search creates for each feature a permuted copy and stops when
one of them is selected.

## Source

Thomas J, Hepp T, Mayr A, Bischl B (2017). “Probing for Sparse and Fast
Variable Selection with Model-Based Boosting.” *Computational and
Mathematical Methods in Medicine*, **2017**, 1–8.
[doi:10.1155/2017/1421409](https://doi.org/10.1155/2017/1421409) .

Wu Y, Boos DD, Stefanski LA (2007). “Controlling Variable Selection by
the Addition of Pseudovariables.” *Journal of the American Statistical
Association*, **102**(477), 235–243.
[doi:10.1198/016214506000000843](https://doi.org/10.1198/016214506000000843)
.

## Details

The feature selection terminates itself when the first shadow variable
is selected. It is not necessary to set a termination criterion.

## Resources

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

- Run a feature selection with [Shadow Variable
  Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).

## Dictionary

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

    fs("shadow_variable_search")

## See also

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md),
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_sequential.md)

## Super classes

[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchShadowVariableSearch`

## Methods

### Public methods

- [`FSelectorBatchShadowVariableSearch$new()`](#method-FSelectorBatchShadowVariableSearch-initialize)

- [`FSelectorBatchShadowVariableSearch$optimization_path()`](#method-FSelectorBatchShadowVariableSearch-optimization_path)

- [`FSelectorBatchShadowVariableSearch$clone()`](#method-FSelectorBatchShadowVariableSearch-clone)

Inherited methods

- [`FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.html#method-optimize)

------------------------------------------------------------------------

### `FSelectorBatchShadowVariableSearch$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.\`

#### Usage

    FSelectorBatchShadowVariableSearch$new()

------------------------------------------------------------------------

### `FSelectorBatchShadowVariableSearch$optimization_path()`

Returns the optimization path.

#### Usage

    FSelectorBatchShadowVariableSearch$optimization_path(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md))  
  Instance optimized with FSelectorBatchShadowVariableSearch.

#### Returns

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### `FSelectorBatchShadowVariableSearch$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchShadowVariableSearch$clone(deep = FALSE)

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
  fselector = fs("shadow_variable_search"),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
)

# best performing feature subset
instance$result
#>    bill_depth bill_length body_mass flipper_length island    sex   year
#>        <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>                      features n_features classif.ce
#>                        <list>      <int>      <num>
#> 1: bill_length,flipper_length          2 0.07826087

# all evaluated feature subsets
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
#>  8:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#>  9:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 10:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 11:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 12:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 13:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 14:      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 15:       TRUE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 16:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 17:      FALSE       FALSE      TRUE           TRUE  FALSE  FALSE  FALSE
#> 18:      FALSE       FALSE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 19:      FALSE       FALSE     FALSE           TRUE  FALSE   TRUE  FALSE
#> 20:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE   TRUE
#> 21:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 22:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 23:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 24:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 25:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 26:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 27:      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 28:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 29:      FALSE        TRUE      TRUE           TRUE  FALSE  FALSE  FALSE
#> 30:      FALSE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 31:      FALSE        TRUE     FALSE           TRUE  FALSE   TRUE  FALSE
#> 32:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE   TRUE
#> 33:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 34:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 35:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 36:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 37:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 38:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 39:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>     classif.ce runtime_learners           timestamp batch_nr
#>          <num>            <num>              <POSc>    <int>
#>  1: 0.32173913            0.016 2026-07-26 10:07:27        1
#>  2: 0.30434783            0.015 2026-07-26 10:07:27        1
#>  3: 0.37391304            0.017 2026-07-26 10:07:27        1
#>  4: 0.28695652            0.016 2026-07-26 10:07:27        1
#>  5: 0.33043478            0.015 2026-07-26 10:07:27        1
#>  6: 0.60869565            0.015 2026-07-26 10:07:27        1
#>  7: 0.60869565            0.015 2026-07-26 10:07:27        1
#>  8: 0.60000000            0.015 2026-07-26 10:07:27        1
#>  9: 0.62608696            0.013 2026-07-26 10:07:27        1
#> 10: 0.57391304            0.015 2026-07-26 10:07:27        1
#> 11: 0.62608696            0.014 2026-07-26 10:07:27        1
#> 12: 0.60869565            0.013 2026-07-26 10:07:27        1
#> 13: 0.60869565            0.013 2026-07-26 10:07:27        1
#> 14: 0.60869565            0.032 2026-07-26 10:07:27        1
#> 15: 0.28695652            0.018 2026-07-26 10:07:27        2
#> 16: 0.07826087            0.015 2026-07-26 10:07:27        2
#> 17: 0.28695652            0.017 2026-07-26 10:07:27        2
#> 18: 0.25217391            0.015 2026-07-26 10:07:27        2
#> 19: 0.28695652            0.017 2026-07-26 10:07:27        2
#> 20: 0.28695652            0.016 2026-07-26 10:07:27        2
#> 21: 0.26956522            0.016 2026-07-26 10:07:27        2
#> 22: 0.28695652            0.016 2026-07-26 10:07:27        2
#> 23: 0.28695652            0.037 2026-07-26 10:07:27        2
#> 24: 0.28695652            0.020 2026-07-26 10:07:27        2
#> 25: 0.28695652            0.016 2026-07-26 10:07:27        2
#> 26: 0.28695652            0.017 2026-07-26 10:07:27        2
#> 27: 0.28695652            0.016 2026-07-26 10:07:27        2
#> 28: 0.07826087            0.017 2026-07-26 10:07:28        3
#> 29: 0.07826087            0.015 2026-07-26 10:07:28        3
#> 30: 0.07826087            0.017 2026-07-26 10:07:28        3
#> 31: 0.07826087            0.015 2026-07-26 10:07:28        3
#> 32: 0.07826087            0.037 2026-07-26 10:07:28        3
#> 33: 0.07826087            0.020 2026-07-26 10:07:28        3
#> 34: 0.07826087            0.016 2026-07-26 10:07:28        3
#> 35: 0.07826087            0.017 2026-07-26 10:07:28        3
#> 36: 0.07826087            0.016 2026-07-26 10:07:28        3
#> 37: 0.07826087            0.016 2026-07-26 10:07:28        3
#> 38: 0.07826087            0.017 2026-07-26 10:07:28        3
#> 39: 0.07826087            0.015 2026-07-26 10:07:28        3
#>     classif.ce runtime_learners           timestamp batch_nr
#>          <num>            <num>              <POSc>    <int>
#>     permuted__bill_depth permuted__bill_length permuted__body_mass
#>                   <lgcl>                <lgcl>              <lgcl>
#>  1:                FALSE                 FALSE               FALSE
#>  2:                FALSE                 FALSE               FALSE
#>  3:                FALSE                 FALSE               FALSE
#>  4:                FALSE                 FALSE               FALSE
#>  5:                FALSE                 FALSE               FALSE
#>  6:                FALSE                 FALSE               FALSE
#>  7:                FALSE                 FALSE               FALSE
#>  8:                 TRUE                 FALSE               FALSE
#>  9:                FALSE                  TRUE               FALSE
#> 10:                FALSE                 FALSE                TRUE
#> 11:                FALSE                 FALSE               FALSE
#> 12:                FALSE                 FALSE               FALSE
#> 13:                FALSE                 FALSE               FALSE
#> 14:                FALSE                 FALSE               FALSE
#> 15:                FALSE                 FALSE               FALSE
#> 16:                FALSE                 FALSE               FALSE
#> 17:                FALSE                 FALSE               FALSE
#> 18:                FALSE                 FALSE               FALSE
#> 19:                FALSE                 FALSE               FALSE
#> 20:                FALSE                 FALSE               FALSE
#> 21:                 TRUE                 FALSE               FALSE
#> 22:                FALSE                  TRUE               FALSE
#> 23:                FALSE                 FALSE                TRUE
#> 24:                FALSE                 FALSE               FALSE
#> 25:                FALSE                 FALSE               FALSE
#> 26:                FALSE                 FALSE               FALSE
#> 27:                FALSE                 FALSE               FALSE
#> 28:                FALSE                 FALSE               FALSE
#> 29:                FALSE                 FALSE               FALSE
#> 30:                FALSE                 FALSE               FALSE
#> 31:                FALSE                 FALSE               FALSE
#> 32:                FALSE                 FALSE               FALSE
#> 33:                 TRUE                 FALSE               FALSE
#> 34:                FALSE                  TRUE               FALSE
#> 35:                FALSE                 FALSE                TRUE
#> 36:                FALSE                 FALSE               FALSE
#> 37:                FALSE                 FALSE               FALSE
#> 38:                FALSE                 FALSE               FALSE
#> 39:                FALSE                 FALSE               FALSE
#>     permuted__bill_depth permuted__bill_length permuted__body_mass
#>                   <lgcl>                <lgcl>              <lgcl>
#>     permuted__flipper_length permuted__island permuted__sex permuted__year
#>                       <lgcl>           <lgcl>        <lgcl>         <lgcl>
#>  1:                    FALSE            FALSE         FALSE          FALSE
#>  2:                    FALSE            FALSE         FALSE          FALSE
#>  3:                    FALSE            FALSE         FALSE          FALSE
#>  4:                    FALSE            FALSE         FALSE          FALSE
#>  5:                    FALSE            FALSE         FALSE          FALSE
#>  6:                    FALSE            FALSE         FALSE          FALSE
#>  7:                    FALSE            FALSE         FALSE          FALSE
#>  8:                    FALSE            FALSE         FALSE          FALSE
#>  9:                    FALSE            FALSE         FALSE          FALSE
#> 10:                    FALSE            FALSE         FALSE          FALSE
#> 11:                     TRUE            FALSE         FALSE          FALSE
#> 12:                    FALSE             TRUE         FALSE          FALSE
#> 13:                    FALSE            FALSE          TRUE          FALSE
#> 14:                    FALSE            FALSE         FALSE           TRUE
#> 15:                    FALSE            FALSE         FALSE          FALSE
#> 16:                    FALSE            FALSE         FALSE          FALSE
#> 17:                    FALSE            FALSE         FALSE          FALSE
#> 18:                    FALSE            FALSE         FALSE          FALSE
#> 19:                    FALSE            FALSE         FALSE          FALSE
#> 20:                    FALSE            FALSE         FALSE          FALSE
#> 21:                    FALSE            FALSE         FALSE          FALSE
#> 22:                    FALSE            FALSE         FALSE          FALSE
#> 23:                    FALSE            FALSE         FALSE          FALSE
#> 24:                     TRUE            FALSE         FALSE          FALSE
#> 25:                    FALSE             TRUE         FALSE          FALSE
#> 26:                    FALSE            FALSE          TRUE          FALSE
#> 27:                    FALSE            FALSE         FALSE           TRUE
#> 28:                    FALSE            FALSE         FALSE          FALSE
#> 29:                    FALSE            FALSE         FALSE          FALSE
#> 30:                    FALSE            FALSE         FALSE          FALSE
#> 31:                    FALSE            FALSE         FALSE          FALSE
#> 32:                    FALSE            FALSE         FALSE          FALSE
#> 33:                    FALSE            FALSE         FALSE          FALSE
#> 34:                    FALSE            FALSE         FALSE          FALSE
#> 35:                    FALSE            FALSE         FALSE          FALSE
#> 36:                     TRUE            FALSE         FALSE          FALSE
#> 37:                    FALSE             TRUE         FALSE          FALSE
#> 38:                    FALSE            FALSE          TRUE          FALSE
#> 39:                    FALSE            FALSE         FALSE           TRUE
#>     permuted__flipper_length permuted__island permuted__sex permuted__year
#>                       <lgcl>           <lgcl>        <lgcl>         <lgcl>
#>     warnings errors                              features n_features
#>        <int>  <int>                                <list>     <list>
#>  1:        0      0                            bill_depth          1
#>  2:        0      0                           bill_length          1
#>  3:        0      0                             body_mass          1
#>  4:        0      0                        flipper_length          1
#>  5:        0      0                                island          1
#>  6:        0      0                                   sex          1
#>  7:        0      0                                  year          1
#>  8:        0      0                                                0
#>  9:        0      0                                                0
#> 10:        0      0                                                0
#> 11:        0      0                                                0
#> 12:        0      0                                                0
#> 13:        0      0                                                0
#> 14:        0      0                                                0
#> 15:        0      0             bill_depth,flipper_length          2
#> 16:        0      0            bill_length,flipper_length          2
#> 17:        0      0              body_mass,flipper_length          2
#> 18:        0      0                 flipper_length,island          2
#> 19:        0      0                    flipper_length,sex          2
#> 20:        0      0                   flipper_length,year          2
#> 21:        0      0                        flipper_length          1
#> 22:        0      0                        flipper_length          1
#> 23:        0      0                        flipper_length          1
#> 24:        0      0                        flipper_length          1
#> 25:        0      0                        flipper_length          1
#> 26:        0      0                        flipper_length          1
#> 27:        0      0                        flipper_length          1
#> 28:        0      0 bill_depth,bill_length,flipper_length          3
#> 29:        0      0  bill_length,body_mass,flipper_length          3
#> 30:        0      0     bill_length,flipper_length,island          3
#> 31:        0      0        bill_length,flipper_length,sex          3
#> 32:        0      0       bill_length,flipper_length,year          3
#> 33:        0      0            bill_length,flipper_length          2
#> 34:        0      0            bill_length,flipper_length          2
#> 35:        0      0            bill_length,flipper_length          2
#> 36:        0      0            bill_length,flipper_length          2
#> 37:        0      0            bill_length,flipper_length          2
#> 38:        0      0            bill_length,flipper_length          2
#> 39:        0      0            bill_length,flipper_length          2
#>     warnings errors                              features n_features
#>        <int>  <int>                                <list>     <list>
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
#> 11: <ResampleResult>
#> 12: <ResampleResult>
#> 13: <ResampleResult>
#> 14: <ResampleResult>
#> 15: <ResampleResult>
#> 16: <ResampleResult>
#> 17: <ResampleResult>
#> 18: <ResampleResult>
#> 19: <ResampleResult>
#> 20: <ResampleResult>
#> 21: <ResampleResult>
#> 22: <ResampleResult>
#> 23: <ResampleResult>
#> 24: <ResampleResult>
#> 25: <ResampleResult>
#> 26: <ResampleResult>
#> 27: <ResampleResult>
#> 28: <ResampleResult>
#> 29: <ResampleResult>
#> 30: <ResampleResult>
#> 31: <ResampleResult>
#> 32: <ResampleResult>
#> 33: <ResampleResult>
#> 34: <ResampleResult>
#> 35: <ResampleResult>
#> 36: <ResampleResult>
#> 37: <ResampleResult>
#> 38: <ResampleResult>
#> 39: <ResampleResult>
#>      resample_result
#>               <list>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
