#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @importFrom R6 R6Class
#' @importFrom utils head
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  mlr_filters <<- DictionaryFilter$new()

  mlr_filters$add("auc", FilterAUC)
  mlr_filters$add("cmim", FilterCMIM)
  mlr_filters$add("disr", FilterDISR)
  mlr_filters$add("gain_ratio", FilterGainRatio)
  mlr_filters$add("information_gain", FilterInformationGain)
  mlr_filters$add("jmi", FilterJMI)
  mlr_filters$add("jmim", FilterJMIM)
  mlr_filters$add("kruskal_test", FilterKruskalTest)
  mlr_filters$add("linear_correlation", FilterLinearCorrelation)
  mlr_filters$add("mim", FilterMIM)
  mlr_filters$add("njmim", FilterNJMIM)
  mlr_filters$add("rank_correlation", FilterRankCorrelation)
  mlr_filters$add("symmetrical_uncertainty", FilterSymmetricalUncertainty)
  mlr_filters$add("variable_importance", FilterVariableImportance, required_args = "learner")
  mlr_filters$add("variance", FilterVariance)
} # nocov end
