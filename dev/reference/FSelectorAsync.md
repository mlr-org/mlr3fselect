# Class for Asynchronous Feature Selection Algorithms

The FSelectorAsync implements the asynchronous optimization algorithm.

## Details

FSelectorAsync is an abstract base class that implements the base
functionality each asynchronous fselector must provide.

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

[`FSelector`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md)
-\> `FSelectorAsync`

## Methods

### Public methods

- [`FSelectorAsync$optimize()`](#method-FSelectorAsync-optimize)

- [`FSelectorAsync$clone()`](#method-FSelectorAsync-clone)

Inherited methods

- [`FSelector$format()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-format)
- [`FSelector$help()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-help)
- [`FSelector$initialize()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-initialize)
- [`FSelector$print()`](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.html#method-print)

------------------------------------------------------------------------

### `FSelectorAsync$optimize()`

Performs the feature selection on a
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)
or
[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md)
until termination. The single evaluations will be written into the
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/ArchiveAsyncFSelect.md)
that resides in the
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)/[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md).
The result will be written into the instance object.

#### Usage

    FSelectorAsync$optimize(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)
  \|
  [FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md)).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### `FSelectorAsync$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
