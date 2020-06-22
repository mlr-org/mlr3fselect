#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @import bbotk
#' @importFrom R6 R6Class
"_PACKAGE"

register_bbotk = function() {
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")

  x$add("evolutionary", OptimizerEvolutionary)
  x$add("exhaustive", OptimizerExhaustive)
  x$add("sequential", OptimizerSequential)
  x$add("rfe", OptimizerRFE)
  x$add("random_binary", OptimizerRandomBinary)
}

.onLoad = function(libname, pkgname) {
  # nocov start

  register_bbotk()
  setHook(packageEvent("bbotk", "onLoad"), function(...) register_bbotk(),
    action = "append")

  assign("lg", lgr::get_logger("mlr3/mlr3fselect"),
    envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
