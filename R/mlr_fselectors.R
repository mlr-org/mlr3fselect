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
#' * `as.data.table(dict)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "properties", "packages" and "man" as columns.
#'
#' @family Dictionary
#' @family FSelector
#' @seealso
#' Sugar functions: [fs()], [fss()]
#'
#' @export
#' @examples
#' as.data.table(mlr_fselectors)
#' mlr_fselectors$get("random_search")
#' fs("random_search")
mlr_fselectors = R6Class("DictionaryFSelector",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryFSelector = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    t = tryCatch(x$get(key),
      missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }

    list(
      key = key,
      properties = list(t$properties),
      packages = list(t$packages),
      man = t$man
    )
  }, .fill = TRUE), "key")[]
}
