
# mlr3fselect

Package website: [release](https://mlr3fselect.mlr-org.com/) |
[dev](https://mlr3fselect.mlr-org.com/dev/)

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3fselect/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3fselect/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version/mlr3fselect)](https://cran.r-project.org/package=mlr3fselect)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3fselect/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3fselect)
<!-- badges: end -->

This package provides feature selection for
[mlr3](https://mlr3.mlr-org.com). It offers various feature selection
wrappers, e.g. random search and sequential feature selection and
different termination criteria can be set and combined. `AutoFSelect`
provides a convenient way to perform nested resampling in combination
with `mlr3`. The package is build on
[bbotk](https://github.com/mlr-org/bbotk) which provides a common
framework for optimization. For feature filters and embedded methods,
see [mlr3filters](https://mlr3filters.mlr-org.com)

## Resources

  - mlr3book
    [chapter](https://mlr3book.mlr-org.com/optimization.html#fs)
  - mlr3gallery
    [post](https://mlr3gallery.mlr-org.com/posts/2020-09-14-mlr3fselect-basic/)
  - [cheatsheet](https://cheatsheets.mlr-org.com/mlr3fselect.pdf)

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3fselect")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3fselect")
```

## Example

### Basic feature selection

``` r
library("mlr3fselect")

# feature selection on the pima indians diabetes data set
instance = fselect(
  method = "random_search",
  task =  tsk("pima"),
  learner = lrn("classif.rpart"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# best performing feature set
instance$result
```

    ##     age glucose insulin mass pedigree pregnant pressure triceps                                  features classif.ce
    ## 1: TRUE    TRUE    TRUE TRUE     TRUE    FALSE    FALSE    TRUE age,glucose,insulin,mass,pedigree,triceps  0.1757812

``` r
# all evaluated feature sets
as.data.table(instance$archive)
```

    ##       age glucose insulin  mass pedigree pregnant pressure triceps classif.ce runtime_learners           timestamp batch_nr      resample_result
    ##  1:  TRUE    TRUE   FALSE  TRUE    FALSE     TRUE     TRUE   FALSE  0.2031250            0.126 2022-01-19 19:23:19        1 <ResampleResult[22]>
    ##  2: FALSE   FALSE    TRUE FALSE    FALSE     TRUE     TRUE   FALSE  0.2578125            0.166 2022-01-19 19:23:19        1 <ResampleResult[22]>
    ##  3:  TRUE    TRUE    TRUE FALSE     TRUE     TRUE    FALSE   FALSE  0.2070312            0.155 2022-01-19 19:23:19        1 <ResampleResult[22]>
    ##  4:  TRUE    TRUE   FALSE  TRUE    FALSE     TRUE    FALSE   FALSE  0.2031250            0.133 2022-01-19 19:23:19        1 <ResampleResult[22]>
    ##  5:  TRUE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE   FALSE  0.2968750            0.116 2022-01-19 19:23:19        1 <ResampleResult[22]>
    ##  6: FALSE    TRUE   FALSE FALSE     TRUE    FALSE    FALSE   FALSE  0.2031250            0.127 2022-01-19 19:23:21        2 <ResampleResult[22]>
    ##  7:  TRUE   FALSE   FALSE FALSE     TRUE     TRUE     TRUE    TRUE  0.3203125            0.183 2022-01-19 19:23:21        2 <ResampleResult[22]>
    ##  8:  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2578125            0.177 2022-01-19 19:23:21        2 <ResampleResult[22]>
    ##  9:  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE    TRUE  0.1757812            0.139 2022-01-19 19:23:21        2 <ResampleResult[22]>
    ## 10:  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2578125            0.196 2022-01-19 19:23:21        2 <ResampleResult[22]>

### Automatic feature selection

``` r
# task
task = tsk("pima")

# construct auto tuner
afs = auto_fselector(
  method = "random_search",
  learner = lrn("classif.rpart"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# train/test split
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

# select features set and fit final model on the complete data set in one go
afs$train(task, row_ids = train_set)

# best performing feature set
afs$fselect_result
```

    ##      age glucose insulin mass pedigree pregnant pressure triceps                                            features classif.ce
    ## 1: FALSE    TRUE    TRUE TRUE     TRUE     TRUE     TRUE    TRUE glucose,insulin,mass,pedigree,pregnant,pressure,...  0.2829268

``` r
# all evaluated feature sets
as.data.table(afs$archive)
```

    ##       age glucose insulin  mass pedigree pregnant pressure triceps classif.ce runtime_learners           timestamp batch_nr      resample_result
    ##  1:  TRUE    TRUE    TRUE  TRUE    FALSE     TRUE    FALSE    TRUE  0.2878049            0.126 2022-01-19 19:23:23        1 <ResampleResult[22]>
    ##  2: FALSE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2829268            0.102 2022-01-19 19:23:23        1 <ResampleResult[22]>
    ##  3:  TRUE   FALSE    TRUE FALSE     TRUE    FALSE    FALSE    TRUE  0.3073171            0.105 2022-01-19 19:23:23        1 <ResampleResult[22]>
    ##  4: FALSE    TRUE    TRUE  TRUE    FALSE    FALSE     TRUE   FALSE  0.2829268            0.122 2022-01-19 19:23:23        1 <ResampleResult[22]>
    ##  5:  TRUE    TRUE    TRUE  TRUE    FALSE     TRUE     TRUE    TRUE  0.2878049            0.115 2022-01-19 19:23:23        1 <ResampleResult[22]>
    ##  6: FALSE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2829268            0.106 2022-01-19 19:23:25        2 <ResampleResult[22]>
    ##  7: FALSE   FALSE    TRUE FALSE    FALSE    FALSE     TRUE   FALSE  0.3658537            0.168 2022-01-19 19:23:25        2 <ResampleResult[22]>
    ##  8: FALSE   FALSE    TRUE  TRUE     TRUE    FALSE    FALSE   FALSE  0.2926829            0.137 2022-01-19 19:23:25        2 <ResampleResult[22]>
    ##  9: FALSE   FALSE   FALSE FALSE    FALSE     TRUE     TRUE   FALSE  0.3658537            0.119 2022-01-19 19:23:25        2 <ResampleResult[22]>
    ## 10: FALSE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE    TRUE  0.3268293            0.230 2022-01-19 19:23:25        2 <ResampleResult[22]>

``` r
# predict new data
afs$predict(task, row_ids = test_set)
```

    ## <PredictionClassif> for 154 observations:
    ##     row_ids truth response
    ##           2   neg      neg
    ##          12   pos      pos
    ##          16   pos      neg
    ## ---                       
    ##         748   neg      neg
    ##         751   pos      pos
    ##         766   neg      neg

### Nested resampling

``` r
# nested resampling
rr = fselect_nested(
  method = "random_search",
  task =  tsk("pima"),
  learner = lrn("classif.rpart"),
  inner_resampling = rsmp("holdout"),
  outer_resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# aggregated performance of all outer resampling iterations
rr$aggregate()
```

    ## classif.ce 
    ##  0.2617188

``` r
# performance scores of the outer resampling
rr$score()
```

    ##                 task task_id             learner              learner_id         resampling resampling_id iteration              prediction classif.ce
    ## 1: <TaskClassif[49]>    pima <AutoFSelector[41]> classif.rpart.fselector <ResamplingCV[19]>            cv         1 <PredictionClassif[20]>  0.2265625
    ## 2: <TaskClassif[49]>    pima <AutoFSelector[41]> classif.rpart.fselector <ResamplingCV[19]>            cv         2 <PredictionClassif[20]>  0.2617188
    ## 3: <TaskClassif[49]>    pima <AutoFSelector[41]> classif.rpart.fselector <ResamplingCV[19]>            cv         3 <PredictionClassif[20]>  0.2968750

``` r
# inner resampling results
extract_inner_fselect_results(rr)
```

    ##    iteration   age glucose insulin  mass pedigree pregnant pressure triceps classif.ce                                       features task_id              learner_id resampling_id
    ## 1:         1  TRUE    TRUE   FALSE  TRUE    FALSE    FALSE    FALSE   FALSE  0.2748538                               age,glucose,mass    pima classif.rpart.fselector            cv
    ## 2:         2  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE     TRUE    TRUE  0.2397661 age,glucose,insulin,mass,pedigree,pressure,...    pima classif.rpart.fselector            cv
    ## 3:         3 FALSE    TRUE   FALSE FALSE    FALSE    FALSE    FALSE   FALSE  0.2222222                                        glucose    pima classif.rpart.fselector            cv

``` r
# inner resampling archives
extract_inner_fselect_archives(rr)
```

    ##     iteration   age glucose insulin  mass pedigree pregnant pressure triceps classif.ce runtime_learners           timestamp batch_nr      resample_result task_id              learner_id
    ##  1:         1  TRUE    TRUE   FALSE  TRUE     TRUE     TRUE     TRUE   FALSE  0.2807018            0.127 2022-01-19 19:23:28        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  2:         1 FALSE    TRUE   FALSE FALSE     TRUE     TRUE     TRUE    TRUE  0.3216374            0.117 2022-01-19 19:23:28        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  3:         1  TRUE   FALSE    TRUE FALSE     TRUE     TRUE     TRUE    TRUE  0.3976608            0.118 2022-01-19 19:23:28        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  4:         1  TRUE   FALSE   FALSE  TRUE     TRUE     TRUE     TRUE    TRUE  0.3450292            0.096 2022-01-19 19:23:28        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  5:         1  TRUE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2865497            0.095 2022-01-19 19:23:28        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  6:         1 FALSE   FALSE   FALSE FALSE    FALSE    FALSE     TRUE   FALSE  0.3567251            0.129 2022-01-19 19:23:29        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  7:         1 FALSE   FALSE    TRUE FALSE    FALSE     TRUE    FALSE   FALSE  0.3918129            0.090 2022-01-19 19:23:29        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  8:         1  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.3742690            0.120 2022-01-19 19:23:29        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ##  9:         1  TRUE    TRUE   FALSE  TRUE    FALSE    FALSE    FALSE   FALSE  0.2748538            0.104 2022-01-19 19:23:29        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 10:         1 FALSE   FALSE   FALSE FALSE    FALSE    FALSE     TRUE   FALSE  0.3567251            0.116 2022-01-19 19:23:29        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 11:         2  TRUE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE   FALSE  0.2456140            0.087 2022-01-19 19:23:30        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 12:         2  TRUE   FALSE   FALSE  TRUE    FALSE    FALSE    FALSE   FALSE  0.3274854            0.098 2022-01-19 19:23:30        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 13:         2  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.3274854            0.084 2022-01-19 19:23:30        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 14:         2 FALSE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE   FALSE  0.2514620            0.097 2022-01-19 19:23:30        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 15:         2 FALSE   FALSE   FALSE FALSE     TRUE     TRUE    FALSE    TRUE  0.4152047            0.080 2022-01-19 19:23:30        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 16:         2  TRUE   FALSE   FALSE FALSE    FALSE     TRUE    FALSE   FALSE  0.3391813            0.083 2022-01-19 19:23:31        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 17:         2 FALSE   FALSE   FALSE FALSE    FALSE    FALSE     TRUE   FALSE  0.3508772            0.107 2022-01-19 19:23:31        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 18:         2  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE     TRUE    TRUE  0.2397661            0.080 2022-01-19 19:23:31        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 19:         2 FALSE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE    TRUE  0.3742690            0.118 2022-01-19 19:23:31        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 20:         2  TRUE   FALSE    TRUE  TRUE    FALSE    FALSE    FALSE   FALSE  0.2807018            0.127 2022-01-19 19:23:31        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 21:         3 FALSE    TRUE   FALSE FALSE    FALSE    FALSE    FALSE   FALSE  0.2222222            0.094 2022-01-19 19:23:33        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 22:         3  TRUE   FALSE    TRUE FALSE    FALSE     TRUE    FALSE   FALSE  0.3157895            0.080 2022-01-19 19:23:33        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 23:         3  TRUE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2456140            0.109 2022-01-19 19:23:33        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 24:         3 FALSE   FALSE   FALSE FALSE     TRUE     TRUE    FALSE   FALSE  0.3040936            0.080 2022-01-19 19:23:33        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 25:         3  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE    TRUE  0.2339181            0.093 2022-01-19 19:23:33        1 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 26:         3  TRUE    TRUE    TRUE FALSE     TRUE     TRUE     TRUE    TRUE  0.3274854            0.136 2022-01-19 19:23:34        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 27:         3  TRUE   FALSE   FALSE  TRUE    FALSE    FALSE    FALSE    TRUE  0.3391813            0.095 2022-01-19 19:23:34        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 28:         3 FALSE   FALSE   FALSE FALSE    FALSE     TRUE    FALSE   FALSE  0.2982456            0.104 2022-01-19 19:23:34        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 29:         3 FALSE    TRUE    TRUE FALSE    FALSE    FALSE     TRUE   FALSE  0.2222222            0.123 2022-01-19 19:23:34        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ## 30:         3 FALSE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE   FALSE  0.2397661            0.130 2022-01-19 19:23:34        2 <ResampleResult[22]>    pima classif.rpart.fselector
    ##     iteration   age glucose insulin  mass pedigree pregnant pressure triceps classif.ce runtime_learners           timestamp batch_nr      resample_result task_id              learner_id
    ##     resampling_id
    ##  1:            cv
    ##  2:            cv
    ##  3:            cv
    ##  4:            cv
    ##  5:            cv
    ##  6:            cv
    ##  7:            cv
    ##  8:            cv
    ##  9:            cv
    ## 10:            cv
    ## 11:            cv
    ## 12:            cv
    ## 13:            cv
    ## 14:            cv
    ## 15:            cv
    ## 16:            cv
    ## 17:            cv
    ## 18:            cv
    ## 19:            cv
    ## 20:            cv
    ## 21:            cv
    ## 22:            cv
    ## 23:            cv
    ## 24:            cv
    ## 25:            cv
    ## 26:            cv
    ## 27:            cv
    ## 28:            cv
    ## 29:            cv
    ## 30:            cv
    ##     resampling_id
