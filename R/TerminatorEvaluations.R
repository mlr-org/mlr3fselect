#' @title TerminatorEvaluations
#'
#' @description
#' Terminator child class to terminate the feature selection if the model performance does not improve to a specified threshold in the next step.
#'
#' @section Usage:
#'  ```
#' tm = TerminatorEvaluations$new()
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `pe` (`[PerformanceEvaluator]`)
#' * `max_evaluations` (`integer(1)`):
#'   Maximum number of function evaluations.
#' * `max_features` (`integer(1)`)
#'   Maximum number of features. Only effects [FeatureSelectionForward].
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorEvaluations].
#'
#' The interface is described in [Terminator].
#'
#' @name TerminatorEvaluations
#' @family Terminator
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorEvaluations$new(pe, max_evaluations = 100)
NULL

#' @export
#' @include Terminator.R
TerminatorEvaluations = R6Class("TerminatorEvaluations",
  inherit = Terminator,
  public = list(
    initialize = function(pe, max_evaluations, max_features = NA) {
      super$initialize(pe = pe, settings = list(max_evaluations = checkmate::assert_count(max_evaluations, positive = TRUE, coerce = TRUE),
                                                max_features = checkmate::assert_numeric(max_features,
                                                                                         lower = 1,
                                                                                         upper = length(pe$task$feature_names))))

      self$state = list(evals = 0L)
      self$terminated = FALSE
    },

    update_start = function() {
      if (length(self$pe$bmr) < 1) {
        self$state$evals = 0L
      } else {
        row_num = lapply(pe$bmr, function(bmr) nrow(bmr$aggregated()))
        self$state$evals = Reduce("sum", row_num)
      }
      self$terminated = (self$state$evals >= self$settings$max_evaluations)

      private$check_maximum_features()

      invisible(self)
    },

    update_end = function() {
      self$update_start()
    }
  )
)
