# One Standard Error Rule Callback

Selects the smallest feature set within one standard error of the best
as the result. If there are multiple such feature sets with the same
number of features, the first one is selected. If the sets have exactly
the same performance but different number of features, the one with the
smallest number of features is selected.

## Source

Kuhn, Max, Johnson, Kjell (2013). “Applied Predictive Modeling.” In
chapter Over-Fitting and Model Tuning, 61–92. Springer New York, New
York, NY. ISBN 978-1-4614-6849-3.

## Examples

``` r
clbk("mlr3fselect.one_se_rule")
#> <CallbackBatchFSelect:mlr3fselect.one_se_rule>: One Standard Error Rule Callback
#> * Active Stages: on_optimization_end

# Run feature selection on the pima data set with the callback
instance = fselect(
  fselector = fs("random_search"),
  task = tsk("pima"),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("cv", folds = 3),
  measures = msr("classif.ce"),
  term_evals = 10,
  callbacks = clbk("mlr3fselect.one_se_rule"))
# Smallest feature set within one standard error of the best
instance$result
#>       age glucose insulin   mass pedigree pregnant pressure triceps
#>    <lgcl>  <lgcl>  <lgcl> <lgcl>   <lgcl>   <lgcl>   <lgcl>  <lgcl>
#> 1:   TRUE    TRUE    TRUE  FALSE    FALSE    FALSE     TRUE    TRUE
#>                                features n_features classif.ce
#>                                  <list>     <list>      <num>
#> 1: age,glucose,insulin,pressure,triceps          5  0.2669271
```
