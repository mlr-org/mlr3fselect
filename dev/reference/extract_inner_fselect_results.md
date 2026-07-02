# Extract Inner Feature Selection Results

Extract inner feature selection results of nested resampling.
Implemented for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
and
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

## Usage

``` r
extract_inner_fselect_results(x, fselect_instance, ...)
```

## Arguments

- x:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  \|
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).

- fselect_instance:

  (`logical(1)`)  
  If `TRUE`, instances are added to the table.

- ...:

  (any)  
  Additional arguments.

## Value

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

## Details

The function iterates over the
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
objects and binds the feature selection results to a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
must be initialized with `store_fselect_instance = TRUE` and
`resample()` or `benchmark()` must be called with `store_models = TRUE`.
Optionally, the instance can be added for each iteration.

## Data structure

The returned data table has the following columns:

- `experiment` (integer(1))  
  Index, giving the according row number in the original benchmark grid.

- `iteration` (integer(1))  
  Iteration of the outer resampling.

- One column for each feature of the task.

- One column for each performance measure.

- `features` (character())  
  Vector of selected feature set.

- `task_id` (`character(1)`).

- `learner_id` (`character(1)`).

- `resampling_id` (`character(1)`).

## Examples

``` r
# Nested Resampling on Palmer Penguins Data Set

# create auto fselector
at = auto_fselector(
  fselector = fs("random_search"),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

resampling_outer = rsmp("cv", folds = 2)
rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)

# extract inner results
extract_inner_fselect_results(rr)
#>    iteration Petal.Length Petal.Width Sepal.Length Sepal.Width classif.ce
#>        <int>       <lgcl>      <lgcl>       <lgcl>      <lgcl>      <num>
#> 1:         1         TRUE       FALSE         TRUE        TRUE       0.04
#> 2:         2        FALSE        TRUE        FALSE       FALSE       0.00
#>                                 features n_features task_id
#>                                   <list>      <int>  <char>
#> 1: Petal.Length,Sepal.Length,Sepal.Width          3    iris
#> 2:                           Petal.Width          1    iris
#>                 learner_id resampling_id
#>                     <char>        <char>
#> 1: classif.rpart.fselector            cv
#> 2: classif.rpart.fselector            cv
```
