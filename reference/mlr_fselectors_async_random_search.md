# Feature Selection with Asynchronous Random Search

Feature selection using Asynchronous Random Search Algorithm.

## Source

Bergstra J, Bengio Y (2012). “Random Search for Hyper-Parameter
Optimization.” *Journal of Machine Learning Research*, **13**(10),
281–305. <https://jmlr.csail.mit.edu/papers/v13/bergstra12a.html>.

## Dictionary

This [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
can be instantiated with the associated sugar function
[`fs()`](https://mlr3fselect.mlr-org.com/reference/fs.md):

    fs("async_random_search")

## Control Parameters

- `max_features`:

  `integer(1)`  
  Maximum number of features. By default, number of features in
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

## See also

Other FSelectorAsync:
[`mlr_fselectors_async_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_async_design_points.md),
[`mlr_fselectors_async_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_async_exhaustive_search.md)

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorAsync`](https://mlr3fselect.mlr-org.com/reference/FSelectorAsync.md)
-\> `FSelectorAsyncRandomSearch`

## Methods

### Public methods

- [`FSelectorAsyncRandomSearch$new()`](#method-FSelectorAsyncRandomSearch-new)

- [`FSelectorAsyncRandomSearch$clone()`](#method-FSelectorAsyncRandomSearch-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)
- [`mlr3fselect::FSelectorAsync$optimize()`](https://mlr3fselect.mlr-org.com/reference/FSelectorAsync.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorAsyncRandomSearch$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsyncRandomSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
