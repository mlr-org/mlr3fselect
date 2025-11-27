# Assertion for mlr3fselect objects

Most assertion functions ensure the right class attribute, and
optionally additional properties.

## Usage

``` r
assert_fselectors(fselectors)

assert_fselector_async(fselector)

assert_fselector_batch(fselector)

assert_fselect_instance(inst)

assert_fselect_instance_async(inst)

assert_fselect_instance_batch(inst)
```

## Arguments

- fselectors:

  (list of
  [FSelector](https://mlr3fselect.mlr-org.com/reference/FSelector.md)).

- fselector:

  ([FSelectorBatch](https://mlr3fselect.mlr-org.com/reference/FSelectorBatch.md)).

- inst:

  ([FSelectInstanceBatchSingleCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchSingleCrit.md)
  \|
  [FSelectInstanceBatchMultiCrit](https://mlr3fselect.mlr-org.com/reference/FSelectInstanceBatchMultiCrit.md)).
