# FSelectorAsyncFromOptimizerAsync

Internally used to transform
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
to
[FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md).

## Super classes

[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\>
[`FSelectorAsync`](https://mlr3fselect.mlr-org.com/dev/reference/FSelectorAsync.md)
-\> `FSelectorAsyncFromOptimizerAsync`

## Active bindings

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

## Methods

### Public methods

- [`FSelectorAsyncFromOptimizerAsync$new()`](#method-FSelectorAsyncFromOptimizerAsync-initialize)

- [`FSelectorAsyncFromOptimizerAsync$optimize()`](#method-FSelectorAsyncFromOptimizerAsync-optimize)

- [`FSelectorAsyncFromOptimizerAsync$clone()`](#method-FSelectorAsyncFromOptimizerAsync-clone)

Inherited methods

- [`FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)

------------------------------------------------------------------------

### `FSelectorAsyncFromOptimizerAsync$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorAsyncFromOptimizerAsync$new(optimizer, man = NA_character_)

#### Arguments

- `optimizer`:

  [bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)  
  Optimizer that is called.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### `FSelectorAsyncFromOptimizerAsync$optimize()`

Performs the feature selection on a
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)
/
[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md)
until termination. The single evaluations and the final results will be
written into the
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveAsyncFSelect.md)
that resides in the
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)/[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md).
The final result is returned.

#### Usage

    FSelectorAsyncFromOptimizerAsync$optimize(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)
  \|
  [FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md)).

#### Returns

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

------------------------------------------------------------------------

### `FSelectorAsyncFromOptimizerAsync$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsyncFromOptimizerAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
