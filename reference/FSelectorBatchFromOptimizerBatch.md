# FSelectorBatchFromOptimizerBatch

Internally used to transform
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
to [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md).

## Super classes

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\>
[`mlr3fselect::FSelectorBatch`](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.md)
-\> `FSelectorBatchFromOptimizerBatch`

## Methods

### Public methods

- [`FSelectorBatchFromOptimizerBatch$new()`](#method-FSelectorBatchFromOptimizerBatch-new)

- [`FSelectorBatchFromOptimizerBatch$optimize()`](#method-FSelectorBatchFromOptimizerBatch-optimize)

- [`FSelectorBatchFromOptimizerBatch$clone()`](#method-FSelectorBatchFromOptimizerBatch-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatchFromOptimizerBatch$new(optimizer, man = NA_character_)

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

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the feature selection on a
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
/
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md)
until termination.

#### Usage

    FSelectorBatchFromOptimizerBatch$optimize(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
  \|
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md)).

#### Returns

[data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatchFromOptimizerBatch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
