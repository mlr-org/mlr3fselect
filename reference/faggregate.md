# Fast Aggregation of ResampleResults and BenchmarkResults

Aggregates a
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
or
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
for a single simple measure. Returns the aggregated score for each
resample result.

## Usage

``` r
faggregate(obj, measure, conditions = FALSE)
```

## Arguments

- obj:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  \|
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- conditions:

  (`logical(1)`)  
  If `TRUE`, the function returns the number of warnings and the number
  of errors.

## Value

([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))

## Details

This function is faster than `$aggregate()` because it does not
reassemble the resampling results. It only works on simple measures
which do not require the task, learner, model or train set to be
available.
