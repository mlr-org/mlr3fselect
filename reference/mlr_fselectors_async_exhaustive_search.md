# Feature Selection with Asynchronous Exhaustive Search

Feature Selection using the Asynchronous Exhaustive Search Algorithm.
Exhaustive Search generates all possible feature sets. The feature sets
are evaluated asynchronously.

## Details

The feature selection terminates itself when all feature sets are
evaluated. It is not necessary to set a termination criterion.

## Dictionary

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("async_exhaustive_search")

## Control Parameters

- `max_features`:

  `integer(1)`  
  Maximum number of features. By default, number of features in
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

## See also

Other FSelectorAsync:
[`mlr_fselectors_async_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_async_design_points.md),
[`mlr_fselectors_async_random_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_async_random_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorAsync`](https://mlr3fselect.mlr-org.com/reference/FSelectorAsync.md)
-\> `FSelectorAsyncExhaustiveSearch`

## Methods

### Public methods

- [`FSelectorAsyncExhaustiveSearch$new()`](#method-FSelectorAsyncExhaustiveSearch-new)

- [`FSelectorAsyncExhaustiveSearch$optimize()`](#method-FSelectorAsyncExhaustiveSearch-optimize)

- [`FSelectorAsyncExhaustiveSearch$clone()`](#method-FSelectorAsyncExhaustiveSearch-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorAsyncExhaustiveSearch$new()

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Starts the asynchronous optimization.

#### Usage

    FSelectorAsyncExhaustiveSearch$optimize(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncSingleCrit.md)
  \|
  [FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncMultiCrit.md)).

#### Returns

[data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsyncExhaustiveSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
