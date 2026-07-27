# Extract Inner Feature Selection Archives

Extract inner feature selection archives of nested resampling.
Implemented for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
and
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).
The function iterates over the
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
objects and binds the archives to a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
[AutoFSelector](https://mlr3fselect.mlr-org.com/dev/reference/AutoFSelector.md)
must be initialized with `store_fselect_instance = TRUE` and
`resample()` or `benchmark()` must be called with `store_models = TRUE`.

## Usage

``` r
extract_inner_fselect_archives(x, exclude_columns = "uhash")
```

## Arguments

- x:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  \|
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).

- exclude_columns:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Exclude columns from result table. Set to `NULL` if no column should
  be excluded.

## Value

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

## Data structure

The returned data table has the following columns:

- `experiment` (integer(1))  
  Index, giving the according row number in the original benchmark grid.

- `iteration` (integer(1))  
  Iteration of the outer resampling.

- One column for each feature of the task.

- One column for each performance measure.

- `runtime_learners` (`numeric(1)`)  
  Sum of training and predict times logged in learners per
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  / evaluation. This does not include potential overhead time.

- `timestamp` (`POSIXct`)  
  Time stamp when the evaluation was logged into the archive.

- `batch_nr` (`integer(1)`)  
  Feature sets are evaluated in batches. Each batch has a unique batch
  number.

- `resample_result`
  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html))  
  Resample result of the inner resampling.

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
rr = resample(tsk("penguins"), at, resampling_outer, store_models = TRUE)

# extract inner archives
extract_inner_fselect_archives(rr)
#>     iteration bill_depth bill_length body_mass flipper_length island    sex
#>         <int>     <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl>
#>  1:         1       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE
#>  2:         1       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE
#>  3:         1      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE
#>  4:         1       TRUE       FALSE      TRUE          FALSE  FALSE  FALSE
#>  5:         1       TRUE        TRUE      TRUE           TRUE  FALSE   TRUE
#>  6:         1      FALSE       FALSE     FALSE           TRUE  FALSE  FALSE
#>  7:         1      FALSE       FALSE     FALSE          FALSE  FALSE   TRUE
#>  8:         1      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE
#>  9:         1       TRUE       FALSE      TRUE          FALSE   TRUE   TRUE
#> 10:         1       TRUE       FALSE     FALSE           TRUE   TRUE   TRUE
#> 11:         2      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE
#> 12:         2       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE
#> 13:         2      FALSE       FALSE     FALSE          FALSE  FALSE   TRUE
#> 14:         2       TRUE        TRUE     FALSE          FALSE  FALSE  FALSE
#> 15:         2      FALSE       FALSE      TRUE          FALSE  FALSE  FALSE
#> 16:         2       TRUE        TRUE     FALSE          FALSE   TRUE   TRUE
#> 17:         2       TRUE       FALSE      TRUE           TRUE   TRUE   TRUE
#> 18:         2       TRUE       FALSE      TRUE          FALSE  FALSE  FALSE
#> 19:         2       TRUE        TRUE     FALSE           TRUE   TRUE  FALSE
#> 20:         2      FALSE       FALSE     FALSE          FALSE   TRUE   TRUE
#>     iteration bill_depth bill_length body_mass flipper_length island    sex
#>         <int>     <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl>
#>       year classif.ce runtime_learners           timestamp batch_nr warnings
#>     <lgcl>      <num>            <num>              <POSc>    <int>    <int>
#>  1:  FALSE 0.08771930            0.007 2026-07-27 10:27:52        1        0
#>  2:   TRUE 0.08771930            0.009 2026-07-27 10:27:52        1        0
#>  3:  FALSE 0.15789474            0.009 2026-07-27 10:27:52        1        0
#>  4:  FALSE 0.31578947            0.007 2026-07-27 10:27:52        1        0
#>  5:   TRUE 0.08771930            0.007 2026-07-27 10:27:52        1        0
#>  6:  FALSE 0.15789474            0.006 2026-07-27 10:27:52        1        0
#>  7:  FALSE 0.57894737            0.006 2026-07-27 10:27:52        1        0
#>  8:  FALSE 0.28070175            0.006 2026-07-27 10:27:52        1        0
#>  9:   TRUE 0.19298246            0.007 2026-07-27 10:27:52        1        0
#> 10:  FALSE 0.24561404            0.007 2026-07-27 10:27:52        1        0
#> 11:  FALSE 0.31578947            0.006 2026-07-27 10:27:52        1        0
#> 12:   TRUE 0.03508772            0.006 2026-07-27 10:27:52        1        0
#> 13:  FALSE 0.59649123            0.005 2026-07-27 10:27:52        1        0
#> 14:   TRUE 0.08771930            0.006 2026-07-27 10:27:52        1        0
#> 15:  FALSE 0.31578947            0.005 2026-07-27 10:27:52        1        0
#> 16:  FALSE 0.05263158            0.005 2026-07-27 10:27:52        1        0
#> 17:  FALSE 0.08771930            0.006 2026-07-27 10:27:52        1        0
#> 18:   TRUE 0.31578947            0.005 2026-07-27 10:27:52        1        0
#> 19:  FALSE 0.03508772            0.006 2026-07-27 10:27:52        1        0
#> 20:  FALSE 0.22807018            0.005 2026-07-27 10:27:52        1        0
#>       year classif.ce runtime_learners           timestamp batch_nr warnings
#>     <lgcl>      <num>            <num>              <POSc>    <int>    <int>
#>     errors                                                          features
#>      <int>                                                            <list>
#>  1:      0        bill_depth,bill_length,body_mass,flipper_length,island,sex
#>  2:      0 bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]
#>  3:      0                                                    flipper_length
#>  4:      0                                              bill_depth,body_mass
#>  5:      0          bill_depth,bill_length,body_mass,flipper_length,sex,year
#>  6:      0                                                    flipper_length
#>  7:      0                                                               sex
#>  8:      0                                                            island
#>  9:      0                              bill_depth,body_mass,island,sex,year
#> 10:      0                              bill_depth,flipper_length,island,sex
#> 11:      0                                                         body_mass
#> 12:      0 bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]
#> 13:      0                                                               sex
#> 14:      0                                       bill_depth,bill_length,year
#> 15:      0                                                         body_mass
#> 16:      0                                 bill_depth,bill_length,island,sex
#> 17:      0                    bill_depth,body_mass,flipper_length,island,sex
#> 18:      0                                         bill_depth,body_mass,year
#> 19:      0                      bill_depth,bill_length,flipper_length,island
#> 20:      0                                                        island,sex
#>     errors                                                          features
#>      <int>                                                            <list>
#>     n_features  resample_result  task_id              learner_id resampling_id
#>         <list>           <list>   <char>                  <char>        <char>
#>  1:          6 <ResampleResult> penguins classif.rpart.fselector            cv
#>  2:          7 <ResampleResult> penguins classif.rpart.fselector            cv
#>  3:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#>  4:          2 <ResampleResult> penguins classif.rpart.fselector            cv
#>  5:          6 <ResampleResult> penguins classif.rpart.fselector            cv
#>  6:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#>  7:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#>  8:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#>  9:          5 <ResampleResult> penguins classif.rpart.fselector            cv
#> 10:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#> 11:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#> 12:          7 <ResampleResult> penguins classif.rpart.fselector            cv
#> 13:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#> 14:          3 <ResampleResult> penguins classif.rpart.fselector            cv
#> 15:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#> 16:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#> 17:          5 <ResampleResult> penguins classif.rpart.fselector            cv
#> 18:          3 <ResampleResult> penguins classif.rpart.fselector            cv
#> 19:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#> 20:          2 <ResampleResult> penguins classif.rpart.fselector            cv
#>     n_features  resample_result  task_id              learner_id resampling_id
#>         <list>           <list>   <char>                  <char>        <char>
```
