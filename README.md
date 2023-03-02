
# mlr3fselect <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3fselect.mlr-org.com/) |
[dev](https://mlr3fselect.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3fselect/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3fselect/actions/workflows/r-cmd-check.yml)
[![CRAN
Status](https://www.r-pkg.org/badges/version/mlr3fselect)](https://cran.r-project.org/package=mlr3fselect)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

*mlr3fselect* is the feature selection package of the
[mlr3](https://mlr-org.com/) ecosystem. It selects the optimal feature
set for any mlr3 [learner](https://github.com/mlr-org/mlr3learners). The
package works with several optimization algorithms e.g. Random Search,
Recursive Feature Elimination, and Genetic Search. Moreover, it can
[automatically](https://mlr3book.mlr-org.com/feature-selection.html#autofselect)
optimize learners and estimate the performance of optimized feature sets
with [nested
resampling](https://mlr3book.mlr-org.com/optimization.html#sec-nested-resampling).
The package is built on the optimization framework
[bbotk](https://github.com/mlr-org/bbotk).

## Resources

There are several section about feature selection in the
[mlr3book](https://mlr3book.mlr-org.com).

  - Getting started with [wrapper feature
    selection](https://mlr3book.mlr-org.com/feature-selection.html#fs-wrapper)
  - Estimate Model Performance with [nested
    resampling](https://mlr3book.mlr-org.com/feature-selection.html#autofselect).

The [gallery](https://mlr-org.com/gallery.html) features a collection of
case studies and demos about optimization.

  - Utilize the built-in feature importance of models with [Recursive
    Feature
    Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).
  - Run a feature selection with [Shadow Variable
    Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).
  - [Feature
    Selection](https://mlr-org.com/gallery/optimization/2020-09-14-mlr3fselect-basic/)
    on the Titanic data set.

The [cheatsheet](https://cheatsheets.mlr-org.com/mlr3fselect.pdf)
summarizes the most important functions of mlr3fselect.

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

We run a feature selection for a support vector machine on the
[Spam](https://mlr3.mlr-org.com/reference/mlr_tasks_spam.html) data set.

``` r
library("mlr3verse")

tsk("spam")
```

    ## <TaskClassif:spam> (4601 x 58): HP Spam Detection
    ## * Target: type
    ## * Properties: twoclass
    ## * Features (57):
    ##   - dbl (57): address, addresses, all, business, capitalAve, capitalLong, capitalTotal,
    ##     charDollar, charExclamation, charHash, charRoundbracket, charSemicolon,
    ##     charSquarebracket, conference, credit, cs, data, direct, edu, email, font, free,
    ##     george, hp, hpl, internet, lab, labs, mail, make, meeting, money, num000, num1999,
    ##     num3d, num415, num650, num85, num857, order, original, our, over, parts, people, pm,
    ##     project, re, receive, remove, report, table, technology, telnet, will, you, your

We construct an instance with the `fsi()` function. The instance
describes the optimization problem.

``` r
instance = fsi(
  task = tsk("spam"),
  learner = lrn("classif.svm", type = "C-classification"),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20)
)
instance
```

    ## <FSelectInstanceSingleCrit>
    ## * State:  Not optimized
    ## * Objective: <ObjectiveFSelect:classif.svm_on_spam>
    ## * Terminator: <TerminatorEvals>

We select a simple random search as the optimization algorithm.

``` r
fselector = fs("random_search", batch_size = 5)
fselector
```

    ## <FSelectorRandomSearch>: Random Search
    ## * Parameters: batch_size=5
    ## * Properties: single-crit, multi-crit
    ## * Packages: mlr3fselect

To start the feature selection, we simply pass the instance to the
fselector.

``` r
fselector$optimize(instance)
```

The fselector writes the best hyperparameter configuration to the
instance.

``` r
instance$result_feature_set
```

    ##  [1] "address"           "addresses"         "all"               "business"         
    ##  [5] "capitalAve"        "capitalLong"       "capitalTotal"      "charDollar"       
    ##  [9] "charExclamation"   "charHash"          "charRoundbracket"  "charSemicolon"    
    ## [13] "charSquarebracket" "conference"        "credit"            "cs"               
    ## [17] "data"              "direct"            "edu"               "email"            
    ## [21] "font"              "free"              "george"            "hp"               
    ## [25] "internet"          "lab"               "labs"              "mail"             
    ## [29] "make"              "meeting"           "money"             "num000"           
    ## [33] "num1999"           "num3d"             "num415"            "num650"           
    ## [37] "num85"             "num857"            "order"             "our"              
    ## [41] "parts"             "people"            "pm"                "project"          
    ## [45] "re"                "receive"           "remove"            "report"           
    ## [49] "table"             "technology"        "telnet"            "will"             
    ## [53] "you"               "your"

And the corresponding measured performance.

``` r
instance$result_y
```

    ## classif.ce 
    ## 0.07042005

The archive contains all evaluated hyperparameter configurations.

``` r
as.data.table(instance$archive)
```

    ##     address addresses   all business capitalAve capitalLong capitalTotal charDollar charExclamation
    ##  1:    TRUE      TRUE  TRUE     TRUE       TRUE        TRUE         TRUE       TRUE            TRUE
    ##  2:    TRUE      TRUE  TRUE    FALSE      FALSE        TRUE         TRUE       TRUE            TRUE
    ##  3:    TRUE      TRUE FALSE    FALSE       TRUE        TRUE         TRUE       TRUE            TRUE
    ##  4:    TRUE      TRUE  TRUE     TRUE       TRUE        TRUE         TRUE       TRUE            TRUE
    ##  5:   FALSE     FALSE FALSE    FALSE      FALSE       FALSE        FALSE       TRUE           FALSE
    ## ---                                                                                                
    ## 16:   FALSE     FALSE FALSE    FALSE      FALSE       FALSE        FALSE      FALSE           FALSE
    ## 17:   FALSE     FALSE FALSE     TRUE       TRUE        TRUE        FALSE      FALSE            TRUE
    ## 18:   FALSE     FALSE  TRUE     TRUE      FALSE       FALSE        FALSE       TRUE           FALSE
    ## 19:    TRUE      TRUE  TRUE     TRUE      FALSE        TRUE         TRUE       TRUE            TRUE
    ## 20:    TRUE     FALSE  TRUE    FALSE      FALSE        TRUE        FALSE       TRUE           FALSE
    ## 56 variables not shown: [charHash, charRoundbracket, charSemicolon, charSquarebracket, conference, credit, cs, data, direct, edu, ...]

We fit a final model with the optimized feature set to make predictions
on new data.

``` r
task = tsk("spam")
learner = lrn("classif.svm", type = "C-classification")

task$select(instance$result_feature_set)
learner$train(task)
```
