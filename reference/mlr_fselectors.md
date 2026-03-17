# Dictionary of FSelectors

A
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md).
Each fselector has an associated help page, see `mlr_fselectors_[id]`.

For a more convenient way to retrieve and construct fselectors, see
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md)/[`fss()`](https://mlr3fselect.mlr-org.com/reference/fs.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with fields "key", "label", "properties" and "packages" as columns. If
  `objects` is set to `TRUE`, the constructed objects are returned in
  the list column named `object`.

## See also

Sugar functions:
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md),
[`fss()`](https://mlr3fselect.mlr-org.com/reference/fs.md)

Other FSelector:
[`FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_shadow_variable_search.md)

## Examples

``` r
as.data.table(mlr_fselectors)
#> Key: <key>
#>                         key                          label
#>                      <char>                         <char>
#>  1:     async_design_points     Asynchronous Design Points
#>  2: async_exhaustive_search Asynchronous Exhaustive Search
#>  3:     async_random_search     Asynchronous Random Search
#>  4:           design_points                  Design Points
#>  5:       exhaustive_search              Exhaustive Search
#>  6:          genetic_search                 Genetic Search
#>  7:           random_search                  Random Search
#>  8:                     rfe  Recursive Feature Elimination
#>  9:                   rfecv  Recursive Feature Elimination
#> 10:              sequential              Sequential Search
#> 11:  shadow_variable_search         Shadow Variable Search
#>                                    properties               packages
#>                                        <list>                 <list>
#>  1: dependencies,single-crit,multi-crit,async mlr3fselect,bbotk,rush
#>  2:              single-crit,multi-crit,async       mlr3fselect,rush
#>  3:                    single-crit,multi-crit            mlr3fselect
#>  4:       dependencies,single-crit,multi-crit      mlr3fselect,bbotk
#>  5:                    single-crit,multi-crit            mlr3fselect
#>  6:                               single-crit     mlr3fselect,genalg
#>  7:                    single-crit,multi-crit            mlr3fselect
#>  8:                single-crit,requires_model            mlr3fselect
#>  9:                single-crit,requires_model            mlr3fselect
#> 10:                               single-crit            mlr3fselect
#> 11:                               single-crit            mlr3fselect
mlr_fselectors$get("random_search")
#> 
#> ── <FSelectorBatchRandomSearch>: Random Search ─────────────────────────────────
#> • Parameters: batch_size=10
#> • Properties: single-crit and multi-crit
#> • Packages: mlr3fselect
fs("random_search")
#> 
#> ── <FSelectorBatchRandomSearch>: Random Search ─────────────────────────────────
#> • Parameters: batch_size=10
#> • Properties: single-crit and multi-crit
#> • Packages: mlr3fselect
```
