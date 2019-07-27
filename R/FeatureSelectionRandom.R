#' @title FeatureSelectionRandom
#'
#' @format [R6::R6Class] inheriting from [FeatureSelection].
#' @include FeatureSelection.R
#'
#' @description
#' Random feature selection wrapper.
#'
#' @section Construction:
#'  ```
#' fs = FeatureSelectionRandom$new(pe, tm, measure, param_vals)
#' ```
#'
#' For arguments, see [FeatureSelection].
#'
#' @section Fields:
#' See [FeatureSelection].
#'
#' @section Methods:
#' See [FeatureSelection] and additionally:
#'
#' * `$get_result()`\cr Returns best feature set.
#'
#' @section Parameter Values:
#' * `max_features` (`integer(1)`)\cr Maximum number of features in set.
#'
#' @name FeatureSelectionRandom
#' @family FeatureSelection
#' @examples
#' task = mlr3::mlr_tasks$get("pima")
#' measure = mlr3::mlr_measures$get(c("classif.acc"))
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#' resampling$instantiate(task)
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorEvaluations$new(max_evaluations = 50)
#' fs = FeatureSelectionRandom$new(pe, tm, measure,
#'                                 param_vals = list(max_features = 4))
#' fs$calculate()
#' fs$get_result()
#' @export
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
