
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
    ##  1:  TRUE    TRUE   FALSE  TRUE    FALSE     TRUE     TRUE   FALSE  0.2031250            0.077 2021-06-08 13:34:44        1 <ResampleResult[20]>
    ##  2:  TRUE   FALSE   FALSE FALSE    FALSE    FALSE    FALSE   FALSE  0.2968750            0.071 2021-06-08 13:34:44        1 <ResampleResult[20]>
    ##  3: FALSE   FALSE    TRUE FALSE    FALSE     TRUE     TRUE   FALSE  0.2578125            0.080 2021-06-08 13:34:44        1 <ResampleResult[20]>
    ##  4:  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2578125            0.053 2021-06-08 13:34:45        2 <ResampleResult[20]>
    ##  5:  TRUE   FALSE    TRUE  TRUE     TRUE     TRUE     TRUE    TRUE  0.2578125            0.055 2021-06-08 13:34:45        2 <ResampleResult[20]>
    ##  6: FALSE    TRUE   FALSE FALSE     TRUE    FALSE    FALSE   FALSE  0.2031250            0.051 2021-06-08 13:34:45        2 <ResampleResult[20]>
    ##  7:  TRUE    TRUE    TRUE  TRUE     TRUE    FALSE    FALSE    TRUE  0.1757812            0.059 2021-06-08 13:34:45        2 <ResampleResult[20]>
    ##  8:  TRUE    TRUE   FALSE  TRUE    FALSE     TRUE    FALSE   FALSE  0.2031250            0.068 2021-06-08 13:34:44        1 <ResampleResult[20]>
    ##  9:  TRUE    TRUE    TRUE FALSE     TRUE     TRUE    FALSE   FALSE  0.2070312            0.077 2021-06-08 13:34:44        1 <ResampleResult[20]>
    ## 10:  TRUE   FALSE   FALSE FALSE     TRUE     TRUE     TRUE    TRUE  0.3203125            0.050 2021-06-08 13:34:45        2 <ResampleResult[20]>
