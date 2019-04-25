
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

The package is currently in development and not yet usable.

## Filters

  - `stats::cor(method = "spearman")`
  - `stats::cor(method = "pearson")`
  - `stats::kruskal.test()`
  - `stats::var()`
  - `AUC` -\> `mlr3measures::measureAUC`
  - `FSelectorRcpp::information.gain(type = "gainratop")`
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

    ##                               id      packages
    ##  1:                    FilterAUC         stats
    ##  2:                   FilterCMIM       praznik
    ##  3:                   FilterDISR       praznik
    ##  4:              FilterGainRatio FSelectorRcpp
    ##  5:        FilterInformationGain FSelectorRcpp
    ##  6:                    FilterJMI       praznik
    ##  7:                   FilterJMIM       praznik
    ##  8:            FilterKruskalTest         stats
    ##  9:      FilterLinearCorrelation         stats
    ## 10:                    FilterMIM       praznik
    ## 11:                  FilterNJMIM       praznik
    ## 12:        FilterRankCorrelation         stats
    ## 13: FilterSymmetricalUncertainty FSelectorRcpp
    ## 14:               FilterVariance         stats
    ##                      feature_types    task_type
    ##  1:                        numeric      classif
    ##  2:         numeric,factor,ordered classif,regr
    ##  3:         numeric,factor,ordered      classif
    ##  4:         numeric,factor,ordered classif,regr
    ##  5:         numeric,factor,ordered classif,regr
    ##  6:         numeric,factor,ordered      classif
    ##  7:         numeric,factor,ordered      classif
    ##  8:                        numeric      classif
    ##  9:                        numeric         regr
    ## 10:         numeric,factor,ordered      classif
    ## 11:         numeric,factor,ordered      classif
    ## 12:                        numeric         regr
    ## 13: numeric,integer,factor,ordered classif,regr
    ## 14:                        numeric classif,regr

#### Methods

  - $calculate(): Calculates Filter values

  - $filter(): filters the task by a given criterion

  - $scores: Filter score values

  - $filtered\_task: Filtered task

### Implemented wrappers

### Implemented embedded methods
