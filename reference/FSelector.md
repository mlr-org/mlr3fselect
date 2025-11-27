# FSelector

The \`FSelector“ implements the optimization algorithm.

## Details

`FSelector` is an abstract base class that implements the base
functionality each fselector must provide.

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

## See also

Other FSelector:
[`mlr_fselectors`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors.md),
[`mlr_fselectors_design_points`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_design_points.md),
[`mlr_fselectors_exhaustive_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_exhaustive_search.md),
[`mlr_fselectors_genetic_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_genetic_search.md),
[`mlr_fselectors_random_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_random_search.md),
[`mlr_fselectors_rfe`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfe.md),
[`mlr_fselectors_rfecv`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_rfecv.md),
[`mlr_fselectors_sequential`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_sequential.md),
[`mlr_fselectors_shadow_variable_search`](https://mlr3fselect.mlr-org.com/reference/mlr_fselectors_shadow_variable_search.md)

## Public fields

- `id`:

  (`character(1)`)  
  Identifier of the object. Used in tables, plot and text output.

## Active bindings

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

## Methods

### Public methods

- [`FSelector$new()`](#method-FSelector-new)

- [`FSelector$format()`](#method-FSelector-format)

- [`FSelector$print()`](#method-FSelector-print)

- [`FSelector$help()`](#method-FSelector-help)

- [`FSelector$clone()`](#method-FSelector-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    FSelector$new(
      id = "fselector",
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

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    FSelector$format(...)

#### Arguments

- `...`:

  (ignored).

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    FSelector$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    FSelector$help()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FSelector$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
