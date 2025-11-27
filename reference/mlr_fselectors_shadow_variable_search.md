# Feature Selection with Shadow Variable Search

Feature selection using the Shadow Variable Search Algorithm. Shadow
variable search creates for each feature a permutated copy and stops
when one of them is selected.

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

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("shadow_variable_search")

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
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_sequential.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.md)
-\> `FSelectorBatchShadowVariableSearch`

## Methods

### Public methods

- [`FSelectorBatchShadowVariableSearch$new()`](#method-FSelectorBatchShadowVariableSearch-new)

- [`FSelectorBatchShadowVariableSearch$optimization_path()`](#method-FSelectorBatchShadowVariableSearch-optimization_path)

- [`FSelectorBatchShadowVariableSearch$clone()`](#method-FSelectorBatchShadowVariableSearch-clone)

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

    FSelectorBatchShadowVariableSearch$new()

------------------------------------------------------------------------

### Method `optimization_path()`

Returns the optimization path.

#### Usage

    FSelectorBatchShadowVariableSearch$optimization_path(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md))  
  Instance optimized with FSelectorBatchShadowVariableSearch.

#### Returns

[data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

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
#> 1:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#>              features n_features classif.ce
#>                <list>      <int>      <num>
#> 1: bill_length,island          2 0.02608696

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
#> 15:       TRUE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 16:      FALSE        TRUE      TRUE          FALSE  FALSE  FALSE  FALSE
#> 17:      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 18:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 19:      FALSE        TRUE     FALSE          FALSE  FALSE   TRUE  FALSE
#> 20:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE   TRUE
#> 21:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 22:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 23:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 24:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 25:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 26:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 27:      FALSE        TRUE     FALSE          FALSE  FALSE  FALSE  FALSE
#> 28:       TRUE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 29:      FALSE        TRUE      TRUE          FALSE   TRUE  FALSE  FALSE
#> 30:      FALSE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 31:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE  FALSE
#> 32:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 33:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 34:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 35:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 36:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 37:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 38:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 39:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE  FALSE
#> 40:       TRUE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 41:      FALSE        TRUE      TRUE          FALSE   TRUE  FALSE   TRUE
#> 42:      FALSE        TRUE     FALSE           TRUE   TRUE  FALSE   TRUE
#> 43:      FALSE        TRUE     FALSE          FALSE   TRUE   TRUE   TRUE
#> 44:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 45:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 46:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 47:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 48:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 49:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#> 50:      FALSE        TRUE     FALSE          FALSE   TRUE  FALSE   TRUE
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>     classif.ce runtime_learners           timestamp batch_nr
#>          <num>            <num>              <POSc>    <int>
#>  1: 0.28695652            0.015 2025-11-27 11:01:31        1
#>  2: 0.19130435            0.014 2025-11-27 11:01:31        1
#>  3: 0.35652174            0.014 2025-11-27 11:01:31        1
#>  4: 0.21739130            0.013 2025-11-27 11:01:31        1
#>  5: 0.33043478            0.014 2025-11-27 11:01:31        1
#>  6: 0.53913043            0.014 2025-11-27 11:01:31        1
#>  7: 0.53913043            0.014 2025-11-27 11:01:31        1
#>  8: 0.59130435            0.012 2025-11-27 11:01:31        1
#>  9: 0.57391304            0.012 2025-11-27 11:01:31        1
#> 10: 0.60869565            0.012 2025-11-27 11:01:31        1
#> 11: 0.59130435            0.011 2025-11-27 11:01:31        1
#> 12: 0.53913043            0.011 2025-11-27 11:01:31        1
#> 13: 0.53913043            0.013 2025-11-27 11:01:31        1
#> 14: 0.53913043            0.012 2025-11-27 11:01:31        1
#> 15: 0.04347826            0.014 2025-11-27 11:01:32        2
#> 16: 0.10434783            0.014 2025-11-27 11:01:32        2
#> 17: 0.04347826            0.014 2025-11-27 11:01:32        2
#> 18: 0.02608696            0.014 2025-11-27 11:01:32        2
#> 19: 0.23478261            0.015 2025-11-27 11:01:32        2
#> 20: 0.17391304            0.014 2025-11-27 11:01:32        2
#> 21: 0.25217391            0.014 2025-11-27 11:01:32        2
#> 22: 0.24347826            0.012 2025-11-27 11:01:32        2
#> 23: 0.19130435            0.014 2025-11-27 11:01:32        2
#> 24: 0.16521739            0.014 2025-11-27 11:01:32        2
#> 25: 0.19130435            0.014 2025-11-27 11:01:32        2
#> 26: 0.19130435            0.014 2025-11-27 11:01:32        2
#> 27: 0.19130435            0.012 2025-11-27 11:01:32        2
#> 28: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 29: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 30: 0.03478261            0.014 2025-11-27 11:01:32        3
#> 31: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 32: 0.02608696            0.015 2025-11-27 11:01:32        3
#> 33: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 34: 0.02608696            0.013 2025-11-27 11:01:32        3
#> 35: 0.02608696            0.012 2025-11-27 11:01:32        3
#> 36: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 37: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 38: 0.02608696            0.015 2025-11-27 11:01:32        3
#> 39: 0.02608696            0.014 2025-11-27 11:01:32        3
#> 40: 0.02608696            0.015 2025-11-27 11:01:32        4
#> 41: 0.02608696            0.012 2025-11-27 11:01:32        4
#> 42: 0.03478261            0.013 2025-11-27 11:01:32        4
#> 43: 0.02608696            0.014 2025-11-27 11:01:32        4
#> 44: 0.02608696            0.013 2025-11-27 11:01:32        4
#> 45: 0.02608696            0.015 2025-11-27 11:01:32        4
#> 46: 0.02608696            0.015 2025-11-27 11:01:32        4
#> 47: 0.02608696            0.014 2025-11-27 11:01:32        4
#> 48: 0.02608696            0.014 2025-11-27 11:01:32        4
#> 49: 0.02608696            0.014 2025-11-27 11:01:32        4
#> 50: 0.02608696            0.014 2025-11-27 11:01:32        4
#>     classif.ce runtime_learners           timestamp batch_nr
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
#> 40:                FALSE                 FALSE               FALSE
#> 41:                FALSE                 FALSE               FALSE
#> 42:                FALSE                 FALSE               FALSE
#> 43:                FALSE                 FALSE               FALSE
#> 44:                 TRUE                 FALSE               FALSE
#> 45:                FALSE                  TRUE               FALSE
#> 46:                FALSE                 FALSE                TRUE
#> 47:                FALSE                 FALSE               FALSE
#> 48:                FALSE                 FALSE               FALSE
#> 49:                FALSE                 FALSE               FALSE
#> 50:                FALSE                 FALSE               FALSE
#>     permuted__bill_depth permuted__bill_length permuted__body_mass
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
#> 40:                    FALSE            FALSE         FALSE          FALSE
#> 41:                    FALSE            FALSE         FALSE          FALSE
#> 42:                    FALSE            FALSE         FALSE          FALSE
#> 43:                    FALSE            FALSE         FALSE          FALSE
#> 44:                    FALSE            FALSE         FALSE          FALSE
#> 45:                    FALSE            FALSE         FALSE          FALSE
#> 46:                    FALSE            FALSE         FALSE          FALSE
#> 47:                     TRUE            FALSE         FALSE          FALSE
#> 48:                    FALSE             TRUE         FALSE          FALSE
#> 49:                    FALSE            FALSE          TRUE          FALSE
#> 50:                    FALSE            FALSE         FALSE           TRUE
#>     permuted__flipper_length permuted__island permuted__sex permuted__year
#>     warnings errors                               features n_features
#>        <int>  <int>                                 <list>     <list>
#>  1:        0      0                             bill_depth          1
#>  2:        0      0                            bill_length          1
#>  3:        0      0                              body_mass          1
#>  4:        0      0                         flipper_length          1
#>  5:        0      0                                 island          1
#>  6:        0      0                                    sex          1
#>  7:        0      0                                   year          1
#>  8:        0      0                                                 0
#>  9:        0      0                                                 0
#> 10:        0      0                                                 0
#> 11:        0      0                                                 0
#> 12:        0      0                                                 0
#> 13:        0      0                                                 0
#> 14:        0      0                                                 0
#> 15:        0      0                 bill_depth,bill_length          2
#> 16:        0      0                  bill_length,body_mass          2
#> 17:        0      0             bill_length,flipper_length          2
#> 18:        0      0                     bill_length,island          2
#> 19:        0      0                        bill_length,sex          2
#> 20:        0      0                       bill_length,year          2
#> 21:        0      0                            bill_length          1
#> 22:        0      0                            bill_length          1
#> 23:        0      0                            bill_length          1
#> 24:        0      0                            bill_length          1
#> 25:        0      0                            bill_length          1
#> 26:        0      0                            bill_length          1
#> 27:        0      0                            bill_length          1
#> 28:        0      0          bill_depth,bill_length,island          3
#> 29:        0      0           bill_length,body_mass,island          3
#> 30:        0      0      bill_length,flipper_length,island          3
#> 31:        0      0                 bill_length,island,sex          3
#> 32:        0      0                bill_length,island,year          3
#> 33:        0      0                     bill_length,island          2
#> 34:        0      0                     bill_length,island          2
#> 35:        0      0                     bill_length,island          2
#> 36:        0      0                     bill_length,island          2
#> 37:        0      0                     bill_length,island          2
#> 38:        0      0                     bill_length,island          2
#> 39:        0      0                     bill_length,island          2
#> 40:        0      0     bill_depth,bill_length,island,year          4
#> 41:        0      0      bill_length,body_mass,island,year          4
#> 42:        0      0 bill_length,flipper_length,island,year          4
#> 43:        0      0            bill_length,island,sex,year          4
#> 44:        0      0                bill_length,island,year          3
#> 45:        0      0                bill_length,island,year          3
#> 46:        0      0                bill_length,island,year          3
#> 47:        0      0                bill_length,island,year          3
#> 48:        0      0                bill_length,island,year          3
#> 49:        0      0                bill_length,island,year          3
#> 50:        0      0                bill_length,island,year          3
#>     warnings errors                               features n_features
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
#> 40: <ResampleResult>
#> 41: <ResampleResult>
#> 42: <ResampleResult>
#> 43: <ResampleResult>
#> 44: <ResampleResult>
#> 45: <ResampleResult>
#> 46: <ResampleResult>
#> 47: <ResampleResult>
#> 48: <ResampleResult>
#> 49: <ResampleResult>
#> 50: <ResampleResult>
#>      resample_result

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
