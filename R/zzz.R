#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @importFrom R6 R6Class
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  assign("lg", lgr::get_logger("mlr3fselect"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
