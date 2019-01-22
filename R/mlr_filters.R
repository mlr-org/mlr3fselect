#' @title Dictionary of Filters
#'
#' @format [R6Class] object
#' @description
#' A simple [Dictionary] storing objects of class [Filter].
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
#' mlr_filters$ids()
#' as.data.table(mlr_filters)
#' mlr_filters$get("classif.featureless")
NULL

#' @include Dictionary.R
DictionaryFilter = R6Class("DictionaryFilter",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_filters = DictionaryFilter$new()

#' @export
as.data.table.DictionaryFilter = function(x, ...) {
  setkeyv(map_dtr(x$ids(), function(id) {
    l = x$get(id)
    list(id = id, packages = list(l$packages))
  }), "id")[]
}
