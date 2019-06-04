
# mlr3featsel

*mlr3featsel* adds filters, feature selection methods and embedded
feature selection methods of algorithms to *mlr3*.

[![Travis build
status](https://travis-ci.org/mlr-org/mlr3featsel.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3featsel)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlr3featsel)](https://cran.r-project.org/package=mlr3featsel)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Coverage
status](https://codecov.io/gh/mlr-org/mlr3featsel/branch/master/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3featsel?branch=master)

## Installation

``` r
remotes::install_github("mlr-org/mlr3featsel")
```

## Filters

  - `stats::cor(method = "spearman")`
  - `stats::cor(method = "pearson")`
  - `stats::kruskal.test()`
  - `stats::var()`
  - `AUC` -\> `Metrics::auc`
  - `FSelectorRcpp::information.gain(type = "gainratio")`
  - `FSelectorRcpp::information.gain(type = "infogain")`
  - `FSelectorRcpp::information.gain(type = "symuncert")`
  - `praznik::CMIM()`
  - `praznik::DISR()`
  - `praznik::JMI()`
  - `praznik::JMIM()`
  - `praznik::MIM()`
  - `praznik::NJMIM()`

<!-- end list -->

``` r
library(mlr3featsel)
as.data.table(mlr_filters)
```

    ##                          id      packages
    ##  1:                     auc       Metrics
    ##  2:                    cmim       praznik
    ##  3:                    disr       praznik
    ##  4:              gain_ratio FSelectorRcpp
    ##  5:        information_gain FSelectorRcpp
    ##  6:                     jmi       praznik
    ##  7:                    jmim       praznik
    ##  8:            kruskal_test         stats
    ##  9:      linear_correlation         stats
    ## 10:                     mim       praznik
    ## 11:                   njmim       praznik
    ## 12:        rank_correlation         stats
    ## 13: symmetrical_uncertainty FSelectorRcpp
    ## 14:     variable_importance            NA
    ## 15:                variance         stats
    ##                              feature_types    task_type
    ##  1:                        integer,numeric      classif
    ##  2:         integer,numeric,factor,ordered classif,regr
    ##  3:         integer,numeric,factor,ordered      classif
    ##  4:         integer,numeric,factor,ordered classif,regr
    ##  5:         integer,numeric,factor,ordered classif,regr
    ##  6:         integer,numeric,factor,ordered      classif
    ##  7:         integer,numeric,factor,ordered      classif
    ##  8:                        integer,numeric      classif
    ##  9:                        integer,numeric         regr
    ## 10:         integer,numeric,factor,ordered      classif
    ## 11:         integer,numeric,factor,ordered      classif
    ## 12:                        integer,numeric         regr
    ## 13: integer,numeric,integer,factor,ordered classif,regr
    ## 14:                                     NA           NA
    ## 15:                        integer,numeric classif,regr

#### Public Methods

  - `.$calculate()`: Calculates Filter values

  - `.$filter()`: filters the task by a given criterion

  - `.$scores`: Filter score values

  - `.$filtered_task`: Filtered task

## Implemented “wrapper methods”

## Algorithm-embedded methods

All learners that support the property “importance” can be used to
create a filter based on their respective implemented variable
importance measure.

``` r
library(mlr3)
library(mlr3learners)
mlr_learners$get("classif.ranger")$properties
```

    ## [1] "importance" "multiclass" "oob_error"  "twoclass"   "weights"

Some learner need to have their variable importance measure activated
during learner creation. For example, to use the “impurity” measure of
Random Forest via the *ranger* package:

``` r
task = mlr_tasks$get("iris")
lrn = mlr_learners$get("classif.ranger", 
  param_vals = list(importance = "impurity"))

filter = FilterVariableImportance$new(learner = lrn)
filter$calculate(task)
```

    ## INFO  [16:27:49.885] Training learner 'classif.ranger' on task 'iris' ...

    ## Error: No importance stored

``` r
head(as.data.table(filter), 3)
```

    ## Error: No filter data available
