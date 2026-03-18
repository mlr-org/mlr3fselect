# Class for Feature Selection Objective

Stores the objective function that estimates the performance of feature
subsets. This class is usually constructed internally by the
[FSelectInstanceAsyncSingleCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncSingleCrit.md)
or
[FSelectInstanceAsyncMultiCrit](https://mlr3fselect.mlr-org.com/dev/reference/FSelectInstanceAsyncMultiCrit.md).

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3fselect::ObjectiveFSelect`](https://mlr3fselect.mlr-org.com/dev/reference/ObjectiveFSelect.md)
-\> `ObjectiveFSelectAsync`

## Methods

### Public methods

- [`ObjectiveFSelectAsync$clone()`](#method-ObjectiveFSelectAsync-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$eval_dt()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_dt)
- [`bbotk::Objective$eval_many()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_many)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3fselect::ObjectiveFSelect$initialize()`](https://mlr3fselect.mlr-org.com/dev/reference/ObjectiveFSelect.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObjectiveFSelectAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
