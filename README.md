
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
different termination criteria can be set and combined.’AutoFSelect’
provides a convenient way to perform nested resampling in combination
with ‘mlr3’. The package is build on
[bbotk](https://github.com/mlr-org/bbotk) which provides a common
framework for optimization. For feature filters and embedded methods,
see [mlr3filters](https://mlr3filters.mlr-org.com)

## Resources

  - mlr3book [chapter](https://mlr3book.mlr-org.com/fs.html)
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
```

    ## Loading required package: mlr3

``` r
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
    ##  1:  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE    TRUE  0.1757812            0.058 2021-06-22 12:12:24        2 <ResampleResult[20]>
    ##  2:  TRUE    TRUE    TRUE FALSE     TRUE     TRUE    FALSE   FALSE  0.2070312            0.080 2021-06-22 12:12:23        1 <ResampleResult[20]>
    ##  3:  TRUE    TRUE   FALSE  TRUE    FALSE     TRUE     TRUE   FALSE  0.2031250            0.093 2021-06-22 12:12:23        1 <ResampleResult[20]>
    ##  4:  TRUE    TRUE   FALSE  TRUE    FALSE     TRUE    FALSE   FALSE  0.2031250            0.077 2021-06-22 12:12:23        1 <ResampleResult[20]>
    ##  5: FALSE   FALSE    TRUE FALSE    FALSE     TRUE     TRUE   FALSE  0.2578125            0.074 2021-06-22 12:12:23        1 <ResampleResult[20]>
    ##  6:  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2578125            0.054 2021-06-22 12:12:24        2 <ResampleResult[20]>
    ##  7:  TRUE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE   FALSE  0.2968750            0.078 2021-06-22 12:12:23        1 <ResampleResult[20]>
    ##  8:  TRUE   FALSE   FALSE FALSE     TRUE     TRUE     TRUE    TRUE  0.3203125            0.054 2021-06-22 12:12:24        2 <ResampleResult[20]>
    ##  9:  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2578125            0.056 2021-06-22 12:12:24        2 <ResampleResult[20]>
    ## 10: FALSE    TRUE   FALSE FALSE     TRUE    FALSE    FALSE   FALSE  0.2031250            0.064 2021-06-22 12:12:24        2 <ResampleResult[20]>

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
    ##  1: FALSE   FALSE   FALSE FALSE    FALSE     TRUE     TRUE   FALSE  0.3658537            0.060 2021-06-22 12:12:25        2 <ResampleResult[20]>
    ##  2: FALSE   FALSE    TRUE  TRUE     TRUE    FALSE    FALSE   FALSE  0.2926829            0.056 2021-06-22 12:12:25        2 <ResampleResult[20]>
    ##  3: FALSE    TRUE    TRUE  TRUE    FALSE    FALSE     TRUE   FALSE  0.2829268            0.050 2021-06-22 12:12:25        1 <ResampleResult[20]>
    ##  4: FALSE   FALSE    TRUE FALSE    FALSE    FALSE     TRUE   FALSE  0.3658537            0.054 2021-06-22 12:12:25        2 <ResampleResult[20]>
    ##  5: FALSE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE    TRUE  0.3268293            0.055 2021-06-22 12:12:25        2 <ResampleResult[20]>
    ##  6:  TRUE   FALSE    TRUE FALSE     TRUE    FALSE    FALSE    TRUE  0.3073171            0.055 2021-06-22 12:12:25        1 <ResampleResult[20]>
    ##  7:  TRUE    TRUE    TRUE  TRUE    FALSE     TRUE    FALSE    TRUE  0.2878049            0.062 2021-06-22 12:12:25        1 <ResampleResult[20]>
    ##  8: FALSE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2829268            0.055 2021-06-22 12:12:25        2 <ResampleResult[20]>
    ##  9: FALSE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2829268            0.080 2021-06-22 12:12:25        1 <ResampleResult[20]>
    ## 10:  TRUE    TRUE    TRUE  TRUE    FALSE     TRUE     TRUE    TRUE  0.2878049            0.055 2021-06-22 12:12:25        1 <ResampleResult[20]>

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
    ## 1: <TaskClassif[47]>    pima <AutoFSelector[39]> classif.rpart.fselector <ResamplingCV[19]>            cv         1 <PredictionClassif[19]>  0.2265625
    ## 2: <TaskClassif[47]>    pima <AutoFSelector[39]> classif.rpart.fselector <ResamplingCV[19]>            cv         2 <PredictionClassif[19]>  0.2617188
    ## 3: <TaskClassif[47]>    pima <AutoFSelector[39]> classif.rpart.fselector <ResamplingCV[19]>            cv         3 <PredictionClassif[19]>  0.2968750

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
    ##  1:         1 FALSE   FALSE   FALSE FALSE    FALSE    FALSE     TRUE   FALSE  0.3567251            0.059 2021-06-22 12:12:27        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  2:         1  TRUE   FALSE   FALSE  TRUE     TRUE     TRUE     TRUE    TRUE  0.3450292            0.055 2021-06-22 12:12:27        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  3:         1  TRUE    TRUE   FALSE  TRUE     TRUE     TRUE     TRUE   FALSE  0.2807018            0.055 2021-06-22 12:12:27        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  4:         1 FALSE   FALSE   FALSE FALSE    FALSE    FALSE     TRUE   FALSE  0.3567251            0.059 2021-06-22 12:12:27        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  5:         1 FALSE   FALSE    TRUE FALSE    FALSE     TRUE    FALSE   FALSE  0.3918129            0.052 2021-06-22 12:12:27        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  6:         1  TRUE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2865497            0.056 2021-06-22 12:12:27        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  7:         1  TRUE   FALSE    TRUE FALSE     TRUE     TRUE     TRUE    TRUE  0.3976608            0.057 2021-06-22 12:12:27        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  8:         1 FALSE    TRUE   FALSE FALSE     TRUE     TRUE     TRUE    TRUE  0.3216374            0.059 2021-06-22 12:12:27        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ##  9:         1  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.3742690            0.062 2021-06-22 12:12:27        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 10:         1  TRUE    TRUE   FALSE  TRUE    FALSE    FALSE    FALSE   FALSE  0.2748538            0.056 2021-06-22 12:12:27        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 11:         2  TRUE   FALSE   FALSE  TRUE    FALSE    FALSE    FALSE   FALSE  0.3274854            0.051 2021-06-22 12:12:28        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 12:         2 FALSE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE   FALSE  0.2514620            0.058 2021-06-22 12:12:28        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 13:         2  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.3274854            0.054 2021-06-22 12:12:28        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 14:         2  TRUE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE   FALSE  0.2456140            0.061 2021-06-22 12:12:28        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 15:         2  TRUE   FALSE    TRUE  TRUE    FALSE    FALSE    FALSE   FALSE  0.2807018            0.052 2021-06-22 12:12:29        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 16:         2 FALSE   FALSE   FALSE FALSE    FALSE    FALSE     TRUE   FALSE  0.3508772            0.055 2021-06-22 12:12:29        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 17:         2  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE     TRUE    TRUE  0.2397661            0.064 2021-06-22 12:12:29        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 18:         2 FALSE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE    TRUE  0.3742690            0.056 2021-06-22 12:12:29        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 19:         2 FALSE   FALSE   FALSE FALSE     TRUE     TRUE    FALSE    TRUE  0.4152047            0.088 2021-06-22 12:12:28        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 20:         2  TRUE   FALSE   FALSE FALSE    FALSE     TRUE    FALSE   FALSE  0.3391813            0.059 2021-06-22 12:12:29        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 21:         3 FALSE    TRUE   FALSE FALSE    FALSE    FALSE    FALSE   FALSE  0.2222222            0.058 2021-06-22 12:12:30        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 22:         3 FALSE   FALSE   FALSE FALSE    FALSE     TRUE    FALSE   FALSE  0.2982456            0.051 2021-06-22 12:12:31        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 23:         3  TRUE    TRUE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2456140            0.056 2021-06-22 12:12:30        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 24:         3 FALSE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE   FALSE  0.2397661            0.069 2021-06-22 12:12:31        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 25:         3  TRUE   FALSE   FALSE  TRUE    FALSE    FALSE    FALSE    TRUE  0.3391813            0.064 2021-06-22 12:12:31        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 26:         3  TRUE    TRUE    TRUE FALSE     TRUE     TRUE     TRUE    TRUE  0.3274854            0.055 2021-06-22 12:12:31        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 27:         3 FALSE   FALSE   FALSE FALSE     TRUE     TRUE    FALSE   FALSE  0.3040936            0.060 2021-06-22 12:12:30        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 28:         3 FALSE    TRUE    TRUE FALSE    FALSE    FALSE     TRUE   FALSE  0.2222222            0.055 2021-06-22 12:12:31        2 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 29:         3  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE    TRUE  0.2339181            0.063 2021-06-22 12:12:30        1 <ResampleResult[20]>    pima classif.rpart.fselector
    ## 30:         3  TRUE   FALSE    TRUE FALSE    FALSE     TRUE    FALSE   FALSE  0.3157895            0.066 2021-06-22 12:12:30        1 <ResampleResult[20]>    pima classif.rpart.fselector
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
