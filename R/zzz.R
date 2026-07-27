#' @import data.table
#' @import checkmate
#' @import cli
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @import bbotk
#' @importFrom R6 R6Class
#' @importFrom utils combn head tail
#' @importFrom stats sd
"_PACKAGE"

# Registers the properties, column roles and packages of mlr3fselect in the reflections of bbotk and mlr3.
# All entries are appended with `unique()` because the function runs again on every `devtools::load_all()`.
register_reflections = function() {
  x = utils::getFromNamespace("bbotk_reflections", ns = "bbotk")
  x$optimizer_properties = unique(c(x$optimizer_properties, "requires_model"))

  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  walk(names(x$task_col_roles), function(task_type) {
    x$task_col_roles[[task_type]] = unique(c(x$task_col_roles[[task_type]], "always_included"))
  })

  x$loaded_packages = unique(c(x$loaded_packages, "mlr3fselect"))
}

.onLoad = function(libname, pkgname) {
  # nocov start
  utils::globalVariables(c("super", "self", "n_features", "errors"))

  register_reflections()

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3fselect.backup", load_callback_backup)
  x$add("mlr3fselect.svm_rfe", load_callback_svm_rfe)
  x$add("mlr3fselect.one_se_rule", load_callback_one_se_rule)
  x$add("mlr3fselect.internal_tuning", load_callback_internal_tuning)
  x$add("mlr3fselect.async_freeze_archive", load_callback_freeze_archive)

  assign("lg", lgr::get_logger("mlr3/bbotk"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

leanify_package()
