#' mlr_fselectors
#'
#' @description
#' A [mlr3misc::Dictionary] storing objects of class [FSelector].
#'
#' @export
mlr_fselectors = R6Class("DictionaryFSelect",
  inherit = Dictionary,
  cloneable = FALSE
)$new()
