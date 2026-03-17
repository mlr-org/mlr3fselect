# Extract Inner Feature Selection Archives

Extract inner feature selection archives of nested resampling.
Implemented for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
and
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).
The function iterates over the
[AutoFSelector](https://mlr3fselect.mlr-org.com/reference/AutoFSelector.md)
objects and binds the archives to a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
[AutoFSelector](https://mlr3fselect.mlr-org.com/reference/AutoFSelector.md)
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
#>  1:         1       TRUE        TRUE     FALSE           TRUE  FALSE   TRUE
#>  2:         1      FALSE       FALSE      TRUE          FALSE  FALSE   TRUE
#>  3:         1      FALSE        TRUE     FALSE           TRUE  FALSE  FALSE
#>  4:         1      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE
#>  5:         1      FALSE       FALSE      TRUE           TRUE   TRUE  FALSE
#>  6:         1      FALSE       FALSE      TRUE          FALSE   TRUE  FALSE
#>  7:         1      FALSE        TRUE      TRUE           TRUE  FALSE   TRUE
#>  8:         1       TRUE        TRUE      TRUE           TRUE   TRUE   TRUE
#>  9:         1       TRUE        TRUE     FALSE          FALSE  FALSE   TRUE
#> 10:         1       TRUE       FALSE     FALSE          FALSE   TRUE   TRUE
#> 11:         2       TRUE       FALSE      TRUE           TRUE   TRUE   TRUE
#> 12:         2       TRUE        TRUE      TRUE           TRUE  FALSE  FALSE
#> 13:         2      FALSE        TRUE     FALSE           TRUE  FALSE   TRUE
#> 14:         2      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE
#> 15:         2      FALSE       FALSE     FALSE          FALSE  FALSE  FALSE
#> 16:         2       TRUE        TRUE      TRUE           TRUE   TRUE  FALSE
#> 17:         2      FALSE       FALSE     FALSE          FALSE   TRUE  FALSE
#> 18:         2       TRUE        TRUE      TRUE           TRUE   TRUE  FALSE
#> 19:         2      FALSE        TRUE     FALSE           TRUE   TRUE  FALSE
#> 20:         2       TRUE        TRUE     FALSE           TRUE  FALSE  FALSE
#>     iteration bill_depth bill_length body_mass flipper_length island    sex
#>         <int>     <lgcl>      <lgcl>    <lgcl>         <lgcl> <lgcl> <lgcl>
#>       year classif.ce runtime_learners           timestamp batch_nr warnings
#>     <lgcl>      <num>            <num>              <POSc>    <int>    <int>
#>  1:  FALSE  0.1052632            0.005 2026-03-17 17:31:11        1        0
#>  2:  FALSE  0.2982456            0.006 2026-03-17 17:31:11        1        0
#>  3:  FALSE  0.1052632            0.005 2026-03-17 17:31:11        1        0
#>  4:  FALSE  0.3684211            0.005 2026-03-17 17:31:11        1        0
#>  5:   TRUE  0.1578947            0.005 2026-03-17 17:31:11        1        0
#>  6:  FALSE  0.2280702            0.006 2026-03-17 17:31:11        1        0
#>  7:   TRUE  0.1052632            0.007 2026-03-17 17:31:11        1        0
#>  8:   TRUE  0.1052632            0.006 2026-03-17 17:31:11        1        0
#>  9:  FALSE  0.1052632            0.006 2026-03-17 17:31:11        1        0
#> 10:   TRUE  0.2982456            0.005 2026-03-17 17:31:11        1        0
#> 11:   TRUE  0.1403509            0.007 2026-03-17 17:31:11        1        0
#> 12:   TRUE  0.1052632            0.008 2026-03-17 17:31:11        1        0
#> 13:   TRUE  0.1052632            0.006 2026-03-17 17:31:11        1        0
#> 14:   TRUE  0.7368421            0.005 2026-03-17 17:31:11        1        0
#> 15:   TRUE  0.7368421            0.006 2026-03-17 17:31:11        1        0
#> 16:   TRUE  0.0877193            0.005 2026-03-17 17:31:11        1        0
#> 17:  FALSE  0.3508772            0.005 2026-03-17 17:31:11        1        0
#> 18:  FALSE  0.0877193            0.006 2026-03-17 17:31:11        1        0
#> 19:  FALSE  0.0877193            0.006 2026-03-17 17:31:11        1        0
#> 20:  FALSE  0.1052632            0.005 2026-03-17 17:31:11        1        0
#>       year classif.ce runtime_learners           timestamp batch_nr warnings
#>     <lgcl>      <num>            <num>              <POSc>    <int>    <int>
#>     errors                                                          features
#>      <int>                                                            <list>
#>  1:      0                         bill_depth,bill_length,flipper_length,sex
#>  2:      0                                                     body_mass,sex
#>  3:      0                                        bill_length,flipper_length
#>  4:      0                                                            island
#>  5:      0                              body_mass,flipper_length,island,year
#>  6:      0                                                  body_mass,island
#>  7:      0                     bill_length,body_mass,flipper_length,sex,year
#>  8:      0 bill_depth,bill_length,body_mass,flipper_length,island,sex,...[7]
#>  9:      0                                        bill_depth,bill_length,sex
#> 10:      0                                        bill_depth,island,sex,year
#> 11:      0               bill_depth,body_mass,flipper_length,island,sex,year
#> 12:      0              bill_depth,bill_length,body_mass,flipper_length,year
#> 13:      0                               bill_length,flipper_length,sex,year
#> 14:      0                                                              year
#> 15:      0                                                              year
#> 16:      0       bill_depth,bill_length,body_mass,flipper_length,island,year
#> 17:      0                                                            island
#> 18:      0            bill_depth,bill_length,body_mass,flipper_length,island
#> 19:      0                                 bill_length,flipper_length,island
#> 20:      0                             bill_depth,bill_length,flipper_length
#>     errors                                                          features
#>      <int>                                                            <list>
#>     n_features  resample_result  task_id              learner_id resampling_id
#>         <list>           <list>   <char>                  <char>        <char>
#>  1:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#>  2:          2 <ResampleResult> penguins classif.rpart.fselector            cv
#>  3:          2 <ResampleResult> penguins classif.rpart.fselector            cv
#>  4:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#>  5:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#>  6:          2 <ResampleResult> penguins classif.rpart.fselector            cv
#>  7:          5 <ResampleResult> penguins classif.rpart.fselector            cv
#>  8:          7 <ResampleResult> penguins classif.rpart.fselector            cv
#>  9:          3 <ResampleResult> penguins classif.rpart.fselector            cv
#> 10:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#> 11:          6 <ResampleResult> penguins classif.rpart.fselector            cv
#> 12:          5 <ResampleResult> penguins classif.rpart.fselector            cv
#> 13:          4 <ResampleResult> penguins classif.rpart.fselector            cv
#> 14:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#> 15:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#> 16:          6 <ResampleResult> penguins classif.rpart.fselector            cv
#> 17:          1 <ResampleResult> penguins classif.rpart.fselector            cv
#> 18:          5 <ResampleResult> penguins classif.rpart.fselector            cv
#> 19:          3 <ResampleResult> penguins classif.rpart.fselector            cv
#> 20:          3 <ResampleResult> penguins classif.rpart.fselector            cv
#>     n_features  resample_result  task_id              learner_id resampling_id
#>         <list>           <list>   <char>                  <char>        <char>
```
