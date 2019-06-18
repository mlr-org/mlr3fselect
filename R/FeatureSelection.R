#' @title Abstract FeatureSelection Class
#'
#' @description `FeatureSelection` class that implements the main functionality each fs must have. A fs is an object that describes the optimization method for choosing the features given within the `[PerformanceEvaluator]` object.
#'
#' @section Usage:
#' ```
#' # Construction
#' fs = FeatureSelectionr$new(id, pe, tm, settings = list())
#'
#' # public members
#' fs$id
#' fs$pe
#' fs$tm
#' fs$settings
#'
#' # public methods
#' fs$calculate()
#' ```
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   The id of the FeatureSelection.
#' * `pe` (`[PerformanceEvaluator]`).
#' * `tm` (`[Terminator]`).
#' * `settings` (`list`):\cr
#'   The settings for the FeatureSelection.
#'
#' @section Details:
#' * `$new()` creates a new object of class `[FeatureSelection]`.
#' * `id` stores an identifier for this `[FeatureSelection]`.
#' * `pe` stores the [PerformanceEvaluator] to optimize.
#' * `tm` stores the `[Terminator]`.
#' * `settings` is a list of settings for this `[FeatureSelection]`.
#' * `calculate()` performs the feature selection, until the budget of the `[Terminator]` in the `[PerformanceEvaluator]` is exhausted.
#' @name FeatureSelection
#' @family FeatureSelection
NULL

#' @export
FeatureSelection = R6Class("FeatureSelection",
   public = list(
      id = NULL,
      pe = NULL,
      tm = NULL,
      settings = NULL,
      state = NULL,

      initialize = function(id, pe, tm, settings = list()) {
         self$id = checkmate::assert_string(id)
         self$pe = checkmate::assert_r6(pe, "PerformanceEvaluator")
         self$tm = checkmate::assert_r6(tm, "Terminator")
         self$settings = checkmate::assert_list(settings, names = "unique")
         },

      calculate = function() {
         while(!self$tm$terminated) {
            private$calculate_step()
         }
      }
  ),
   private = list(
      calculate_step = function() {
         states = private$generate_states()
         named_states = lapply(states, private$binary_to_features)

         private$eval_states_terminator(named_states)

         bmr = self$pe$get_best()
         features = bmr[[length(bmr)]]$features
         self$state = as.numeric(Reduce("|", lapply(features, function(x) x == self$pe$task$feature_names)))
      },
      binary_to_features = function(binary_features) {
         task$feature_names[as.logical(binary_features)]
      },
      eval_states_terminator = function(states) {
         self$tm$update_start(self$pe)
         self$pe$eval_states(states)
         self$tm$update_end(self$pe)
      }
 )
)
