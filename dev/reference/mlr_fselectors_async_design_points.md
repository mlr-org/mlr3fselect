# Feature Selection with Asynchronous Design Points

Subclass for asynchronous design points feature selection.

## Dictionary

This
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/dev/reference/fs.md):

    fs("async_design_points")

## Parameters

- `design`:

  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Design points to try in search, one per row.

## See also

Other FSelectorAsync:
[`mlr_fselectors_async_exhaustive_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_async_exhaustive_search.md),
[`mlr_fselectors_async_random_search`](https://mlr3fselect.mlr-org.com/dev/reference/mlr_fselectors_async_random_search.md)

## Super classes

[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`FSelectorAsync`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorAsync.md)
-\>
[`FSelectorAsyncFromOptimizerAsync`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorAsyncFromOptimizerAsync.md)
-\> `FSelectorAsyncDesignPoints`

## Methods

### Public methods

- [`FSelectorAsyncDesignPoints$new()`](#method-FSelectorAsyncDesignPoints-initialize)

- [`FSelectorAsyncDesignPoints$clone()`](#method-FSelectorAsyncDesignPoints-clone)

Inherited methods

- [`FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)
- [`FSelectorAsyncFromOptimizerAsync$optimize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorAsyncFromOptimizerAsync.html#method-optimize)

------------------------------------------------------------------------

### `FSelectorAsyncDesignPoints$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorAsyncDesignPoints$new()

------------------------------------------------------------------------

### `FSelectorAsyncDesignPoints$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsyncDesignPoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
