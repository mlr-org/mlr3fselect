# mlr3fselect

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mlr3fselect/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3fselect/actions)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3fselect)](https://cran.r-project.org/package=mlr3fselect)
[![codecov](https://codecov.io/gh/mlr-org/mlr3fselect/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3fselect)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
<!-- badges: end -->

This package provides feature selection for mlr3. It offers various feature
selection methods e.g. random search and sequential feature selection and
different termination criteria can be set and combined. 'AutoFSelect' provides a
convenient way to perform nested resampling in combination with 'mlr3'. The
package is build on bbotk which provides a common framework for optimization.

## Installation

CRAN version

```{r}
install.packages("mlr3tuning")
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

# Create tuning instance
instance = FSelectInstanceSingleCrit$new(task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  terminator = terminator)

# Load tuner
fselector = fs("random_search")

# Trigger optimization
fselector$optimize(instance)

# View results
instance$result
```


