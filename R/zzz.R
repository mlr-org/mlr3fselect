#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @importFrom R6 R6Class
#' @importFrom utils head
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # this is to silence note "publish_registered_filters: no visible binding for global variable ‘mlr_filter_register’" during R CMD check
  utils::globalVariables("mlr_filter_register")

  publish_registered_filters() # create and fill mlr_filters Dictionary

} # nocov end
