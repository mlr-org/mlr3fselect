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

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.md)
-\> `FSelectorBatchShadowVariableSearch`

## Methods

### Public methods

- [`FSelectorBatchShadowVariableSearch$new()`](#method-FSelectorBatchShadowVariableSearch-new)

- [`FSelectorBatchShadowVariableSearch$optimization_path()`](#method-FSelectorBatchShadowVariableSearch-optimization_path)

- [`FSelectorBatchShadowVariableSearch$clone()`](#method-FSelectorBatchShadowVariableSearch-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorBatch$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorBatch.html#method-optimize)

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

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md))  
  Instance optimized with FSelectorBatchShadowVariableSearch.

#### Returns

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)

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
#> 1:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#>                                 features n_features classif.ce
#>                                   <list>      <int>      <num>
#> 1: bill_depth,bill_length,flipper_length          3 0.04347826

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
#> 40:       TRUE        TRUE      TRUE           TRUE  FALSE  FALSE  FALSE
#> 41:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 42:       TRUE        TRUE     FALSE           TRUE  FALSE   TRUE  FALSE
#> 43:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE   TRUE
#> 44:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 45:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 46:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 47:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 48:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 49:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 50:       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE  FALSE
#> 51:       TRUE        TRUE      TRUE           TRUE   TRUE  FALSE  FALSE
#> 52:       TRUE        TRUE     FALSE           TRUE   TRUE   TRUE  FALSE
#> 53:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE   TRUE
#> 54:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 55:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 56:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 57:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 58:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 59:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#> 60:       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE  FALSE
#>     bill_depth bill_length body_mass flipper_length island    sex   year
#>         <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl> <lgcl>
#>     classif.ce runtime_learners           timestamp batch_nr
#>          <num>            <num>              <POSc>    <int>
#>  1: 0.29565217            0.016 2026-03-19 10:30:38        1
#>  2: 0.29565217            0.016 2026-03-19 10:30:38        1
#>  3: 0.29565217            0.016 2026-03-19 10:30:38        1
#>  4: 0.26956522            0.017 2026-03-19 10:30:38        1
#>  5: 0.32173913            0.015 2026-03-19 10:30:38        1
#>  6: 0.56521739            0.015 2026-03-19 10:30:38        1
#>  7: 0.56521739            0.013 2026-03-19 10:30:38        1
#>  8: 0.56521739            0.012 2026-03-19 10:30:38        1
#>  9: 0.65217391            0.013 2026-03-19 10:30:38        1
#> 10: 0.67826087            0.012 2026-03-19 10:30:38        1
#> 11: 0.58260870            0.013 2026-03-19 10:30:38        1
#> 12: 0.65217391            0.013 2026-03-19 10:30:38        1
#> 13: 0.56521739            0.013 2026-03-19 10:30:38        1
#> 14: 0.56521739            0.012 2026-03-19 10:30:38        1
#> 15: 0.21739130            0.016 2026-03-19 10:30:39        2
#> 16: 0.10434783            0.016 2026-03-19 10:30:39        2
#> 17: 0.28695652            0.016 2026-03-19 10:30:39        2
#> 18: 0.20869565            0.014 2026-03-19 10:30:39        2
#> 19: 0.21739130            0.015 2026-03-19 10:30:39        2
#> 20: 0.26956522            0.015 2026-03-19 10:30:39        2
#> 21: 0.26956522            0.015 2026-03-19 10:30:39        2
#> 22: 0.26956522            0.015 2026-03-19 10:30:39        2
#> 23: 0.33043478            0.015 2026-03-19 10:30:39        2
#> 24: 0.26086957            0.015 2026-03-19 10:30:39        2
#> 25: 0.26956522            0.017 2026-03-19 10:30:39        2
#> 26: 0.26956522            0.017 2026-03-19 10:30:39        2
#> 27: 0.26956522            0.021 2026-03-19 10:30:39        2
#> 28: 0.05217391            0.017 2026-03-19 10:30:39        3
#> 29: 0.10434783            0.016 2026-03-19 10:30:39        3
#> 30: 0.09565217            0.015 2026-03-19 10:30:39        3
#> 31: 0.05217391            0.015 2026-03-19 10:30:39        3
#> 32: 0.10434783            0.015 2026-03-19 10:30:39        3
#> 33: 0.10434783            0.015 2026-03-19 10:30:39        3
#> 34: 0.09565217            0.014 2026-03-19 10:30:39        3
#> 35: 0.10434783            0.014 2026-03-19 10:30:39        3
#> 36: 0.10434783            0.015 2026-03-19 10:30:39        3
#> 37: 0.10434783            0.015 2026-03-19 10:30:39        3
#> 38: 0.10434783            0.035 2026-03-19 10:30:39        3
#> 39: 0.10434783            0.017 2026-03-19 10:30:39        3
#> 40: 0.05217391            0.016 2026-03-19 10:30:40        4
#> 41: 0.04347826            0.016 2026-03-19 10:30:40        4
#> 42: 0.05217391            0.015 2026-03-19 10:30:40        4
#> 43: 0.05217391            0.015 2026-03-19 10:30:40        4
#> 44: 0.05217391            0.015 2026-03-19 10:30:40        4
#> 45: 0.04347826            0.015 2026-03-19 10:30:40        4
#> 46: 0.05217391            0.014 2026-03-19 10:30:40        4
#> 47: 0.05217391            0.016 2026-03-19 10:30:40        4
#> 48: 0.05217391            0.015 2026-03-19 10:30:40        4
#> 49: 0.05217391            0.016 2026-03-19 10:30:40        4
#> 50: 0.05217391            0.039 2026-03-19 10:30:40        4
#> 51: 0.04347826            0.016 2026-03-19 10:30:40        5
#> 52: 0.04347826            0.017 2026-03-19 10:30:40        5
#> 53: 0.04347826            0.015 2026-03-19 10:30:40        5
#> 54: 0.04347826            0.015 2026-03-19 10:30:40        5
#> 55: 0.04347826            0.015 2026-03-19 10:30:40        5
#> 56: 0.04347826            0.016 2026-03-19 10:30:40        5
#> 57: 0.04347826            0.016 2026-03-19 10:30:40        5
#> 58: 0.04347826            0.015 2026-03-19 10:30:40        5
#> 59: 0.04347826            0.016 2026-03-19 10:30:40        5
#> 60: 0.04347826            0.015 2026-03-19 10:30:40        5
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
#> 51:                FALSE                 FALSE               FALSE
#> 52:                FALSE                 FALSE               FALSE
#> 53:                FALSE                 FALSE               FALSE
#> 54:                 TRUE                 FALSE               FALSE
#> 55:                FALSE                  TRUE               FALSE
#> 56:                FALSE                 FALSE                TRUE
#> 57:                FALSE                 FALSE               FALSE
#> 58:                FALSE                 FALSE               FALSE
#> 59:                FALSE                 FALSE               FALSE
#> 60:                FALSE                 FALSE               FALSE
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
#> 51:                    FALSE            FALSE         FALSE          FALSE
#> 52:                    FALSE            FALSE         FALSE          FALSE
#> 53:                    FALSE            FALSE         FALSE          FALSE
#> 54:                    FALSE            FALSE         FALSE          FALSE
#> 55:                    FALSE            FALSE         FALSE          FALSE
#> 56:                    FALSE            FALSE         FALSE          FALSE
#> 57:                     TRUE            FALSE         FALSE          FALSE
#> 58:                    FALSE             TRUE         FALSE          FALSE
#> 59:                    FALSE            FALSE          TRUE          FALSE
#> 60:                    FALSE            FALSE         FALSE           TRUE
#>     permuted__flipper_length permuted__island permuted__sex permuted__year
#>                       <lgcl>           <lgcl>        <lgcl>         <lgcl>
#>     warnings errors                                               features
#>        <int>  <int>                                                 <list>
#>  1:        0      0                                             bill_depth
#>  2:        0      0                                            bill_length
#>  3:        0      0                                              body_mass
#>  4:        0      0                                         flipper_length
#>  5:        0      0                                                 island
#>  6:        0      0                                                    sex
#>  7:        0      0                                                   year
#>  8:        0      0                                                       
#>  9:        0      0                                                       
#> 10:        0      0                                                       
#> 11:        0      0                                                       
#> 12:        0      0                                                       
#> 13:        0      0                                                       
#> 14:        0      0                                                       
#> 15:        0      0                              bill_depth,flipper_length
#> 16:        0      0                             bill_length,flipper_length
#> 17:        0      0                               body_mass,flipper_length
#> 18:        0      0                                  flipper_length,island
#> 19:        0      0                                     flipper_length,sex
#> 20:        0      0                                    flipper_length,year
#> 21:        0      0                                         flipper_length
#> 22:        0      0                                         flipper_length
#> 23:        0      0                                         flipper_length
#> 24:        0      0                                         flipper_length
#> 25:        0      0                                         flipper_length
#> 26:        0      0                                         flipper_length
#> 27:        0      0                                         flipper_length
#> 28:        0      0                  bill_depth,bill_length,flipper_length
#> 29:        0      0                   bill_length,body_mass,flipper_length
#> 30:        0      0                      bill_length,flipper_length,island
#> 31:        0      0                         bill_length,flipper_length,sex
#> 32:        0      0                        bill_length,flipper_length,year
#> 33:        0      0                             bill_length,flipper_length
#> 34:        0      0                             bill_length,flipper_length
#> 35:        0      0                             bill_length,flipper_length
#> 36:        0      0                             bill_length,flipper_length
#> 37:        0      0                             bill_length,flipper_length
#> 38:        0      0                             bill_length,flipper_length
#> 39:        0      0                             bill_length,flipper_length
#> 40:        0      0        bill_depth,bill_length,body_mass,flipper_length
#> 41:        0      0           bill_depth,bill_length,flipper_length,island
#> 42:        0      0              bill_depth,bill_length,flipper_length,sex
#> 43:        0      0             bill_depth,bill_length,flipper_length,year
#> 44:        0      0                  bill_depth,bill_length,flipper_length
#> 45:        0      0                  bill_depth,bill_length,flipper_length
#> 46:        0      0                  bill_depth,bill_length,flipper_length
#> 47:        0      0                  bill_depth,bill_length,flipper_length
#> 48:        0      0                  bill_depth,bill_length,flipper_length
#> 49:        0      0                  bill_depth,bill_length,flipper_length
#> 50:        0      0                  bill_depth,bill_length,flipper_length
#> 51:        0      0 bill_depth,bill_length,body_mass,flipper_length,island
#> 52:        0      0       bill_depth,bill_length,flipper_length,island,sex
#> 53:        0      0      bill_depth,bill_length,flipper_length,island,year
#> 54:        0      0           bill_depth,bill_length,flipper_length,island
#> 55:        0      0           bill_depth,bill_length,flipper_length,island
#> 56:        0      0           bill_depth,bill_length,flipper_length,island
#> 57:        0      0           bill_depth,bill_length,flipper_length,island
#> 58:        0      0           bill_depth,bill_length,flipper_length,island
#> 59:        0      0           bill_depth,bill_length,flipper_length,island
#> 60:        0      0           bill_depth,bill_length,flipper_length,island
#>     warnings errors                                               features
#>        <int>  <int>                                                 <list>
#>     n_features  resample_result
#>         <list>           <list>
#>  1:          1 <ResampleResult>
#>  2:          1 <ResampleResult>
#>  3:          1 <ResampleResult>
#>  4:          1 <ResampleResult>
#>  5:          1 <ResampleResult>
#>  6:          1 <ResampleResult>
#>  7:          1 <ResampleResult>
#>  8:          0 <ResampleResult>
#>  9:          0 <ResampleResult>
#> 10:          0 <ResampleResult>
#> 11:          0 <ResampleResult>
#> 12:          0 <ResampleResult>
#> 13:          0 <ResampleResult>
#> 14:          0 <ResampleResult>
#> 15:          2 <ResampleResult>
#> 16:          2 <ResampleResult>
#> 17:          2 <ResampleResult>
#> 18:          2 <ResampleResult>
#> 19:          2 <ResampleResult>
#> 20:          2 <ResampleResult>
#> 21:          1 <ResampleResult>
#> 22:          1 <ResampleResult>
#> 23:          1 <ResampleResult>
#> 24:          1 <ResampleResult>
#> 25:          1 <ResampleResult>
#> 26:          1 <ResampleResult>
#> 27:          1 <ResampleResult>
#> 28:          3 <ResampleResult>
#> 29:          3 <ResampleResult>
#> 30:          3 <ResampleResult>
#> 31:          3 <ResampleResult>
#> 32:          3 <ResampleResult>
#> 33:          2 <ResampleResult>
#> 34:          2 <ResampleResult>
#> 35:          2 <ResampleResult>
#> 36:          2 <ResampleResult>
#> 37:          2 <ResampleResult>
#> 38:          2 <ResampleResult>
#> 39:          2 <ResampleResult>
#> 40:          4 <ResampleResult>
#> 41:          4 <ResampleResult>
#> 42:          4 <ResampleResult>
#> 43:          4 <ResampleResult>
#> 44:          3 <ResampleResult>
#> 45:          3 <ResampleResult>
#> 46:          3 <ResampleResult>
#> 47:          3 <ResampleResult>
#> 48:          3 <ResampleResult>
#> 49:          3 <ResampleResult>
#> 50:          3 <ResampleResult>
#> 51:          5 <ResampleResult>
#> 52:          5 <ResampleResult>
#> 53:          5 <ResampleResult>
#> 54:          4 <ResampleResult>
#> 55:          4 <ResampleResult>
#> 56:          4 <ResampleResult>
#> 57:          4 <ResampleResult>
#> 58:          4 <ResampleResult>
#> 59:          4 <ResampleResult>
#> 60:          4 <ResampleResult>
#>     n_features  resample_result
#>         <list>           <list>

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
# }
```
