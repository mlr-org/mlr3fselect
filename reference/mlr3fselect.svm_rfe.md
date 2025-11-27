# SVM-RFE Callback

Runs a recursive feature elimination with a
[mlr3learners::LearnerClassifSVM](https://mlr3learners.mlr-org.com/reference/mlr_learners_classif.svm.html).
The SVM must be configured with `type = "C-classification"` and
`kernel = "linear"`.

## Source

Guyon I, Weston J, Barnhill S, Vapnik V (2002). “Gene Selection for
Cancer Classification using Support Vector Machines.” *Machine
Learning*, **46**(1), 389–422. ISSN 1573-0565,
[doi:10.1023/A:1012487302797](https://doi.org/10.1023/A%3A1012487302797)
.

## Examples

``` r
clbk("mlr3fselect.svm_rfe")
#> <CallbackBatchFSelect:mlr3fselect.svm_rfe>: SVM-RFE Callback
#> * Active Stages: on_optimization_begin

library(mlr3learners)

# Create instance with classification svm with linear kernel
instance = fsi(
  task = tsk("sonar"),
  learner = lrn("classif.svm", type = "C-classification", kernel = "linear"),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("none"),
  callbacks = clbk("mlr3fselect.svm_rfe"),
  store_models = TRUE
)

fselector = fs("rfe", feature_number = 5, n_features = 10)

# Run recursive feature elimination on the Sonar data set
fselector$optimize(instance)
#>        V1    V10    V11    V12    V13    V14    V15    V16    V17    V18    V19
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE  FALSE   TRUE   TRUE  FALSE   TRUE
#>        V2    V20    V21    V22    V23    V24    V25    V26    V27    V28    V29
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:  FALSE  FALSE  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE  FALSE
#>        V3    V30    V31    V32    V33    V34    V35    V36    V37    V38    V39
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:   TRUE   TRUE   TRUE   TRUE   TRUE  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE
#>        V4    V40    V41    V42    V43    V44    V45    V46    V47    V48    V49
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:   TRUE   TRUE  FALSE   TRUE   TRUE   TRUE  FALSE   TRUE  FALSE   TRUE   TRUE
#>        V5    V50    V51    V52    V53    V54    V55    V56    V57    V58    V59
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:  FALSE   TRUE   TRUE   TRUE  FALSE   TRUE   TRUE   TRUE  FALSE   TRUE  FALSE
#>        V6    V60     V7     V8     V9
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:   TRUE  FALSE   TRUE   TRUE   TRUE
#>                                                   importance
#>                                                       <list>
#> 1: 41.66667,37.00000,37.00000,34.00000,33.00000,32.66667,...
#>                      features n_features classif.ce
#>                        <list>      <int>      <num>
#> 1: V1,V10,V11,V12,V13,V14,...         45  0.2257419
```
