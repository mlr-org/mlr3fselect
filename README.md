# mlr3fselect

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mlr3fselect/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3fselect/actions)
[![CRAN Status](https://www.r-pkg.org/badges/version/mlr3fselect)](https://cran.r-project.org/package=mlr3fselect)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3fselect/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3fselect)
<!-- badges: end -->

This package provides feature selection for [mlr3](https://mlr3.mlr-org.com).
It offers various feature selection wrappers, e.g. random search and sequential feature selection and
different termination criteria can be set and combined. 'AutoFSelect' provides a
convenient way to perform nested resampling in combination with 'mlr3'. The
package is build on [bbotk](https://github.com/mlr-org/bbotk) which provides a
common framework for optimization.

For feature filters and embedded methods, see [mlr3filters](https://mlr3filters.mlr-org.com)

## Installation

CRAN version

```{r}
install.packages("mlr3fselect")
```

Development version

``` r
remotes::install_github("mlr-org/mlr3fselect")
```

## Example

```{r}
library("mlr3")
library("mlr3fselect")

task = tsk("pima")
learner = lrn("classif.rpart")
resampling = rsmp("holdout")
measure = msr("classif.ce")

# Define termination criterion
terminator = trm("evals", n_evals = 20)

# Create fselect instance
instance = FSelectInstanceSingleCrit$new(task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  terminator = terminator)

# Load fselector
fselector = fs("random_search")

# Trigger optimization
fselector$optimize(instance)

# View results
instance$result
```


