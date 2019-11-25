#' Dictionary of Terminators
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [Terminator].
#' Each terminator has an associated help page, see `mlr_terminators_[id]`.
#'
#' This dictionary can get populated with additional terminators by add-on packages.
#'
#' For a more convenient way to retrieve and construct terminator, see [term()].
#'
#' @seealso
#' Sugar function: [term()]
#' @export
#' @examples
#' term("evals", n_evals = 10)
mlr_terminators = R6Class("DictionaryTerminator",
  inherit = Dictionary,
  cloneable = FALSE
)$new()
