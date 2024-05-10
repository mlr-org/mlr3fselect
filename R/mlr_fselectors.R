#' @title Dictionary of FSelectors
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A [mlr3misc::Dictionary] storing objects of class [FSelector].
#' Each fselector has an associated help page, see `mlr_fselectors_[id]`.
#'
#' For a more convenient way to retrieve and construct fselectors, see [fs()]/[fss()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict, ..., objects = FALSE)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "label", "properties" and "packages" as columns.
#'   If `objects` is set to `TRUE`, the constructed objects are returned in the list column named `object`.
#'
#' @family Dictionary
#' @family FSelector
#' @seealso
#' Sugar functions: [fs()], [fss()]
#'
#' @export
#' @examples
#' as.data.table(mlr_fselectors)x
#' mlr_fselectors$get("random_search")
#' fs("random_search")
mlr_fselectors = R6Class("DictionaryFSelector",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryFSelector = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    t = withCallingHandlers(x$get(key),
      packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = t$label, properties = list(t$properties), packages = list(t$packages)),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
