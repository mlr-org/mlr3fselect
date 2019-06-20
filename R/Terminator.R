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
#' * `pe` (`PerformanceEvaluator`):
#'   `PerformanceEvaluator` object used to determine when to stop the tuning.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Terminator].
#' * `$terminated` (`logical(1)`) is the termination criterion met? Updated by each call of `update_start()`/`update_end()`.
#' * `$settings` (`list()`) settings that are set by the child classes to define stopping criteria.
#' * `$state` (`list()`) arbitrary state of the Terminator. Gets updated with each call of `update_start()` and `update_end()`.
#' * `$update_start()` is called in each tuning iteration before the evaluation.
#' * `$update_end()` is called in each tuning iteration after the evaluation.
#' * `$set_enable_maximum_features` is called by FeatureSelection classes to enable check for maximum features.
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
    pe = NULL,

    initialize = function(pe, settings) {
      self$settings = checkmate::assert_list(settings, names = "unique")
      self$pe = checkmate::assert_r6(pe, "PerformanceEvaluator")

      private$enable_maximum_features = FALSE
    },

    update_start = function() {
      stop("$update_start() not implemented for Terminator")
    },
    update_end = function() {
      stop("$update_end() not implemented for Terminator")
    },
    set_enable_maximum_features = function(value) {
      private$enable_maximum_features = TRUE
    }
   ),
  private = list(
    enable_maximum_features = NULL,
    check_maximum_features = function() {
      # Set to feature length if no max_feature is provided
      if(private$enable_maximum_features) {
        if(is.na(self$settings$max_features)) {
          self$settings$max_features <- length(self$pe$task$feature_names)
        }
        bmr = self$pe$get_best()
        if(length(self$pe$bmr) == self$settings$max_features) {
          self$terminated = TRUE
        }
      }
    }
  )
)
