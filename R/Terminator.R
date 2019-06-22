#' @title Abstract Terminator Class
#'
#' @description Abstract `Terminator` class that implements the main functionality each terminator must have. A terminator is an object that says when to stop the feature selection.
#'
#' @section Usage:
#' ```
#' # Construction
#' tm = Terminator$new()
#'
#' # Public members
#' tm$terminated
#' tm$state
#'
#' # Public methods
#' tm$update_state(pe)
#' tm$update_end(pe)
#' ```
#'
#' @section Arguments:
#' *`settings` (`list(0)`)
#'
#' @section Details:
#' * `$new()` creates a new object of class [Terminator].
#' * `$terminated` (`logical(1)`) is the termination criterion met? Updated by each call of `update_start()`/`update_end()`.
#' * `$settings` (`list()`) settings that are set by the child classes to define stopping criteria.
#' * `$state` (`list()`) arbitrary state of the Terminator. Gets updated with each call of `update_start()` and `update_end()`.
#' * `$update_start()` is called in each tuning iteration before the evaluation.
#' * `$update_end()` is called in each tuning iteration after the evaluation.
#' @name Terminator
#' @keywords internal
#' @family Terminator
NULL

#' @export
Terminator = R6Class("Terminator",
  public = list(
    terminated = NULL,
    settings = NULL,
    state = NULL,

    initialize = function(settings) {
      self$settings = checkmate::assert_list(settings, names = "unique")
    },

    update_start = function(pe) {
      stop("$update_start() not implemented for Terminator")
    },
    update_end = function(pe) {
      stop("$update_end() not implemented for Terminator")
    }
  )
)
