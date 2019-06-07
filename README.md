
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

### Public Methods

  - `.$calculate()`: Calculates Filter values

  - `.$filter()`: filters the task by a given criterion

  - `.$scores`: Filter score values

  - `.$filtered_task`: Filtered task

### Generic Filters

| Name                     | Package         | Features                          | Task           |
| :----------------------- | :-------------- | :-------------------------------- | :------------- |
| auc                      | *Metrics*       | Integer, Numeric                  | Classif        |
| disr                     | *praznik*       | Integer, Numeric, Factor, Ordered | Classif        |
| jmi                      | *praznik*       | Integer, Numeric, Factor, Ordered | Classif        |
| jmim                     | *praznik*       | Integer, Numeric, Factor, Ordered | Classif        |
| kruskal\_test            | *stats*         | Integer, Numeric                  | Classif        |
| mim                      | *praznik*       | Integer, Numeric, Factor, Ordered | Classif        |
| njmim                    | *praznik*       | Integer, Numeric, Factor, Ordered | Classif        |
| cmim                     | *praznik*       | Integer, Numeric, Factor, Ordered | Classif & Regr |
| gain\_ratio              | *FSelectorRcpp* | Integer, Numeric, Factor, Ordered | Classif & Regr |
| information\_gain        | *FSelectorRcpp* | Integer, Numeric, Factor, Ordered | Classif & Regr |
| symmetrical\_uncertainty | *FSelectorRcpp* | Integer, Numeric, Factor, Ordered | Classif & Regr |
| variance                 | *stats*         | Integer, Numeric                  | Classif & Regr |
| variable\_importance     | *NA*            | NA                                | NA             |
| linear\_correlation      | *stats*         | Integer, Numeric                  | Regr           |
| rank\_correlation        | *stats*         | Integer, Numeric                  | Regr           |

### Embedded Filters

The following learners have embedded filter methods which are supported
via class `FilterVariableImportance`:

    ## [1] "classif.featureless" "classif.ranger"      "classif.rpart"      
    ## [4] "classif.xgboost"     "regr.featureless"    "regr.ranger"        
    ## [7] "regr.rpart"          "regr.xgboost"

If your learner is listed here, the reason is most likely that it is not
integrated into [mlr3learners](https://github.com/mlr-org/mlr3learners)
or [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners).
Please open an issue so we can add your package.

Some learners need to have their variable importance measure “activated”
during learner creation. For example, to use the “impurity” measure of
Random Forest via the *ranger* package:

``` r
task = mlr_tasks$get("iris")
lrn = mlr_learners$get("classif.ranger",
  param_vals = list(importance = "impurity"))

filter = FilterVariableImportance$new(learner = lrn)
filter$calculate(task)
```

    ## INFO  [15:36:19.726] Training learner 'classif.ranger' on task 'iris' ...

``` r
head(as.data.table(filter), 3)
```

    ##            name     value
    ## 1:  Petal.Width 44.300510
    ## 2: Petal.Length 42.870670
    ## 3: Sepal.Length  9.786203

## “Wrapper” Methods

Work in progress.
