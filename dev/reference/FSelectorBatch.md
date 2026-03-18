# Class for Batch Feature Selection Algorithms

The FSelectorBatch implements the optimization algorithm.

## Details

FSelectorBatch is an abstract base class that implements the base
functionality each fselector must provide. A subclass is implemented in
the following way:

- Inherit from FSelectorBatch.

- Specify the private abstract method `$.optimize()` and use it to call
  into your optimizer.

- You need to call `instance$eval_batch()` to evaluate design points.

- The batch evaluation is requested at the
  [FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)/[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)
  object `instance`, so each batch is possibly executed in parallel via
  [`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html),
  and all evaluations are stored inside of `instance$archive`.

- Before the batch evaluation, the
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  is checked, and if it is positive, an exception of class
  `"terminated_error"` is generated. In the latter case the current
  batch of evaluations is still stored in `instance`, but the numeric
  scores are not sent back to the handling optimizer as it has lost
  execution control.

- After such an exception was caught we select the best set from
  `instance$archive` and return it.

- Note that therefore more points than specified by the
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  may be evaluated, as the Terminator is only checked before a batch
  evaluation, and not in-between evaluation in a batch. How many more
  depends on the setting of the batch size.

- Overwrite the private super-method `.assign_result()` if you want to
  decide how to estimate the final set in the instance and its estimated
  performance. The default behavior is: We pick the best resample
  experiment, regarding the given measure, then assign its set and
  aggregated performance to the instance.

## Private Methods

- `.optimize(instance)` -\> `NULL`  
  Abstract base method. Implement to specify feature selection of your
  subclass. See technical details sections.

- `.assign_result(instance)` -\> `NULL`  
  Abstract base method. Implement to specify how the final feature
  subset is selected. See technical details sections.

## Resources

There are several sections about feature selection in the
[mlr3book](https://mlr3book.mlr-org.com).

- Learn more about
  [fselectors](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#the-fselector-class).

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

- Utilize the built-in feature importance of models with [Recursive
  Feature
  Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).

- Run a feature selection with [Shadow Variable
  Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).

## Super class

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\> `FSelectorBatch`

## Methods

### Public methods

- [`FSelectorBatch$new()`](#method-FSelectorBatch-new)

- [`FSelectorBatch$optimize()`](#method-FSelectorBatch-optimize)

- [`FSelectorBatch$clone()`](#method-FSelectorBatch-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelectorBatch$new(
      id = "fselector_batch",
      param_set,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `param_set`:

  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)  
  Set of control parameters.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the fselector. Must be a subset of
  [`mlr_reflections$fselect_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. Note that these packages will be loaded via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), and are
  not attached.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the feature selection on a
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
or
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)
until termination. The single evaluations will be written into the
[ArchiveBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveBatchFSelect.md)
that resides in the
[FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
/
[FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md).
The result will be written into the instance object.

#### Usage

    FSelectorBatch$optimize(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchSingleCrit.md)
  \|
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceBatchMultiCrit.md)).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorBatch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
