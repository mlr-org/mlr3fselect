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

[`mlr3fselect::FSelector`](https://mlr3fselect.mlr-org.com/reference/FSelector.md)
-\> `FSelectorAsync`

## Methods

### Public methods

- [`FSelectorAsync$optimize()`](#method-FSelectorAsync-optimize)

- [`FSelectorAsync$clone()`](#method-FSelectorAsync-clone)

Inherited methods

- [`mlr3fselect::FSelector$format()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-format)
- [`mlr3fselect::FSelector$help()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-help)
- [`mlr3fselect::FSelector$initialize()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-initialize)
- [`mlr3fselect::FSelector$print()`](https://mlr3fselect.mlr-org.com/reference/FSelector.html#method-print)

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the feature selection on a
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncSingleCrit.md)
or
[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncMultiCrit.md)
until termination. The single evaluations will be written into the
[ArchiveAsyncFSelect](https://mlr3fselect.mlr-org.com/reference/ArchiveAsyncFSelect.md)
that resides in the
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncSingleCrit.md)/[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncMultiCrit.md).
The result will be written into the instance object.

#### Usage

    FSelectorAsync$optimize(inst)

#### Arguments

- `inst`:

  ([FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncSingleCrit.md)
  \|
  [FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceAsyncMultiCrit.md)).

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelectorAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
