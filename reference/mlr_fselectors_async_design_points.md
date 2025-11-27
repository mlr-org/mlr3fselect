# Feature Selection with Asynchronous Design Points

Subclass for asynchronous design points feature selection.

## Dictionary

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("async_design_points")

## Parameters

- `design`:

  [data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Design points to try in search, one per row.

## See also

Other FSelectorAsync:
[`mlr_fselectors_async_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_async_exhaustive_search.md),
[`mlr_fselectors_async_random_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_async_random_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorAsync`](https://mlr3fselect.mlr-org.com/reference/FSelectorAsync.md)
-\>
[`mlr3fselect::FSelectorAsyncFromOptimizerAsync`](https://mlr3fselect.mlr-org.com/reference/FSelectorAsyncFromOptimizerAsync.md)
-\> `FSelectorAsyncDesignPoints`

## Methods

### Public methods

- [`FSelectorAsyncDesignPoints$new()`](#method-FSelectorAsyncDesignPoints-new)

- [`FSelectorAsyncDesignPoints$clone()`](#method-FSelectorAsyncDesignPoints-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorAsyncFromOptimizerAsync$optimize()`](https://mlr3fselect.mlr-org.com/reference/FSelectorAsyncFromOptimizerAsync.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorAsyncDesignPoints$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsyncDesignPoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
