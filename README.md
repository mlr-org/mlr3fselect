
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

| Name                     | Package                                                     | Features                          | Task           |
| :----------------------- | :---------------------------------------------------------- | :-------------------------------- | :------------- |
| cmim                     | <span style="  font-style: italic;   ">praznik</span>       | Integer, Numeric, Factor, Ordered | Classif & Regr |
| gain\_ratio              | <span style="  font-style: italic;   ">FSelectorRcpp</span> | Integer, Numeric, Factor, Ordered | Classif & Regr |
| information\_gain        | <span style="  font-style: italic;   ">FSelectorRcpp</span> | Integer, Numeric, Factor, Ordered | Classif & Regr |
| symmetrical\_uncertainty | <span style="  font-style: italic;   ">FSelectorRcpp</span> | Integer, Numeric, Factor, Ordered | Classif & Regr |
| variance                 | <span style="  font-style: italic;   ">stats</span>         | Integer, Numeric                  | Classif & Regr |
| auc                      | <span style="  font-style: italic;   ">Metrics</span>       | Integer, Numeric                  | Classif        |
| disr                     | <span style="  font-style: italic;   ">praznik</span>       | Integer, Numeric, Factor, Ordered | Classif        |
| jmi                      | <span style="  font-style: italic;   ">praznik</span>       | Integer, Numeric, Factor, Ordered | Classif        |
| jmim                     | <span style="  font-style: italic;   ">praznik</span>       | Integer, Numeric, Factor, Ordered | Classif        |
| kruskal\_test            | <span style="  font-style: italic;   ">stats</span>         | Integer, Numeric                  | Classif        |
| mim                      | <span style="  font-style: italic;   ">praznik</span>       | Integer, Numeric, Factor, Ordered | Classif        |
| njmim                    | <span style="  font-style: italic;   ">praznik</span>       | Integer, Numeric, Factor, Ordered | Classif        |
| linear\_correlation      | <span style="  font-style: italic;   ">stats</span>         | Integer, Numeric                  | Regr           |
| rank\_correlation        | <span style="  font-style: italic;   ">stats</span>         | Integer, Numeric                  | Regr           |
| variable\_importance     | <span style="  font-style: italic;   ">NA</span>            | NA                                | NA             |

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

    ## INFO  [18:25:33.405] Training learner 'classif.ranger' on task 'iris' ...

``` r
head(as.data.table(filter), 3)
```

    ##            name    value
    ## 1: Petal.Length 43.58996
    ## 2:  Petal.Width 43.14815
    ## 3: Sepal.Length 10.09467
