
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

| Name                     | Task Type      | Task Properties | Parameter Set | Feature Types                                         | Package         |
| :----------------------- | :------------- | :-------------- | :------------ | :---------------------------------------------------- | :-------------- |
| auc                      | Classif        | twoclass        | <environment> | Integer, Numeric                                      | *Metrics*       |
| disr                     | Classif        | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *praznik*       |
| jmi                      | Classif        | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *praznik*       |
| kruskal\_test            | Classif        | character(0)    | <environment> | Integer, Numeric                                      | *stats*         |
| mim                      | Classif        | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *praznik*       |
| njmim                    | Classif        | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *praznik*       |
| variable\_importance     | Classif        | character(0)    | <environment> | Logical, Integer, Numeric, Character, Factor, Ordered | *rpart*         |
| cmim                     | Classif & Regr | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *praznik*       |
| gain\_ratio              | Classif & Regr | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *FSelectorRcpp* |
| information\_gain        | Classif & Regr | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *FSelectorRcpp* |
| symmetrical\_uncertainty | Classif & Regr | character(0)    | <environment> | Integer, Numeric, Factor, Ordered                     | *FSelectorRcpp* |
| variance                 | Classif & Regr | character(0)    | <environment> | Integer, Numeric                                      | *stats*         |
| linear\_correlation      | Regr           | character(0)    | <environment> | Integer, Numeric                                      | *stats*         |
| rank\_correlation        | Regr           | character(0)    | <environment> | Integer, Numeric                                      | *stats*         |

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

    ## INFO  [13:41:25.182] Training learner 'classif.ranger' on task 'iris' ...

``` r
head(as.data.table(filter), 3)
```

    ##       score      feature              method
    ## 1: 43.67502 Petal.Length variable_importance
    ## 2: 42.75024  Petal.Width variable_importance
    ## 3: 10.37421 Sepal.Length variable_importance

## “Wrapper” Methods

Work in progress.
