#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @import bbotk
#' @importFrom R6 R6Class
#' @importFrom utils combn head
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3fselect.backup", load_callback_backup)

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

leanify_package()
