#' @title Dictionary of Filters
#'
#' @format [R6Class] object
#' @description
#' A simple [Dictionary] storing objects of class [FilterResult].
#' Each Filter has an associated help page, see `mlr_filters_[id]`.
#'
#' @section Usage:
#'
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Filter
#' @name mlr_filters
#' @examples
#' mlr_filters$keys()
#' as.data.table(mlr_filters)
#' mlr_filters$get("mim")
NULL

DictionaryFilterResult = R6Class("DictionaryFilterResult",
  inherit = mlr3misc::Dictionary,
  cloneable = FALSE
)

#' @export
mlr_filters = NULL

#' @export
as.data.table.DictionaryFilterResult = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    l = x$get(key)
    list(
      key = key,
      task_type = list(l$task_type),
      task_properties = list(l$task_properties),
      param_set = list(l$param_set),
      feature_types = list(l$feature_types),
      packages = list(l$packages)
    )
  }), "key")[]
}

# We would like to have the filters in the "mlr_filters" Dictionary, but adding
# them at build time is apparently not a good idea. On the other hand we would
# like to register filters near their definition to prevent confusing
# dependencies throughout the codebase. Therefore we register the filters using
# "register_filter()" below their definition and call
# "publish_registered_filters()" in .onLoad. (adapted from mlr3pipelines)
mlr_filter_register = new.env(parent = emptyenv())

# nocov start
register_filter = function(key, value) {
  m = match.call(expand.dots = FALSE)
  mlr_filter_register[[key]] = m
}

publish_registered_filters = function() {
  mlr_filters <<- DictionaryFilterResult$new()

  for (registercall in as.list(mlr_filter_register)) {
    registercall[[1]] = quote(mlr_filters$add)
    eval(registercall, envir = parent.env(environment()))
  }
  rm("mlr_filter_register", envir = parent.env(environment()))
}
# nocov end
