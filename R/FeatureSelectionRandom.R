#' @title FeatureSelectionRandom
#'
#' @description
#' FeatureSelection child class to conduct random search
#'
#' @section Usage:
#'  ```
#' fs = FeatureSelectionRandom$new()
#' ```
#' See [FeatureSelection] for a description of the interface.
#'
#' @section Arguments:
#' * `pe` (`[PerformanceEvaluator]`).
#' * `tm` (`[Terminator]`).
#' * `max_features` (`integer(1)`)
#'   Maximum number of features
#' * `batch_size` (`integer(1`):
#'   Maximum number of feature combinations to try in a batch.
#'   Each batch is possibly executed in parallel via [mlr3::benchmark()].
#'
#' @section Details:
#' `$new()` creates a new object of class [FeatureSelectionRandom].
#' `$get_result()` Returns best feature combination.
#' The interface is described in [FeatureSelection].
#'
#' @name FeatureSelectionRandom
#' @family FeatureSelection
#' @examples
#' task = mlr3::mlr_tasks$get("boston_housing")
#' learner = mlr3::mlr_learners$get("regr.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#' pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
#' tm = TerminatorEvaluations$new(max_evaluations = 20)
#' fs = FeatureSelectionRandom$new(pe, tm, batch_size = 10, max_features = 8)
#' fs$calculate()
#' fs$get_result()
NULL

#' @export
#' @include FeatureSelection.R

FeatureSelectionRandom = R6Class("FeatureSelectionRandom",
  inherit = FeatureSelection,
  public = list(
    initialize = function(pe, tm, measure, param_vals = list()) {
      super$initialize(id = "random_selection",
                       pe = pe,
                       tm = tm,
                       measure = measure,
                       param_vals = param_vals)

      self$state = private$generate_states()
    },
    get_result = function() {
      bmr = self$pe$bmr[[1]]$clone()
      if (length(self$pe$bmr) > 1) {
        lapply(self$pe$bmr[2:length(self$pe$bmr)], function(x) bmr$combine(x))
      }
      bmr_best = bmr$best(self$measure)
      list(
        features = bmr_best$task$feature_names,
        performance = bmr_best$aggregate(self$measure))
    }
  ),
  private = list(
    calculate_step = function() {

      # Convert 0/1 states to feature names
      named_states = lapply(self$state, private$binary_to_features)

      # Evaluation
      private$eval_states_terminator(named_states)

      # Generate new states
      self$state = private$generate_states()
    },
    generate_states = function() {
      lapply(seq_len(10), function(i) {
        if (is.na(self$param_set$values$max_features)) {
          x = 0
          while(sum(x) == 0) {
            x = rbinom(length(self$pe$task$feature_names), 1, 0.5)
          }
          return(x)
        }
        x = Inf
        while (sum(x) > self$param_set$values$max_features | sum(x) == 0) {
          x = rbinom(length(self$pe$task$feature_names), 1, 0.5)
        }
        return(x)
      }
      )
    }
  )
)
