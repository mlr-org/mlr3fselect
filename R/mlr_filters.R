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
#' mlr_filters$keys()
#' as.data.table(mlr_filters)
#' mlr_filters$get("mim")
NULL

DictionaryFilter = R6Class("DictionaryFilter",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_filters = NULL

#' @export
as.data.table.DictionaryFilter = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(id) {
    if (length(x$required_args(id)) > 0L) {
      return(list(id = id, packages = list(NA_character_), feature_types = list(NA_character_), task_type = list(NA_character_)))
    }
    l = x$get(id)
    list(id = id, packages = list(l$packages), feature_types = list(l$feature_types),
      task_type = list(l$task_type))
  }), "id")[]
}
