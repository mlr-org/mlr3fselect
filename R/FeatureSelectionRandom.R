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
#' pe = PerformanceEvaluator$new(task = task,learner = learner, resampling = resampling)
#' tm = TerminatorEvaluations$new(pe, max_evaluations = 20)
#' fs = FeatureSelectionRandom$new(pe, tm, batch_size = 10, max_features = 8)
#' fs$calculate()
#' fs$get_result()
NULL

#' @export
#' @include FeatureSelection.R

FeatureSelectionRandom = R6Class("FeatureSelectionRandom",
  inherit = FeatureSelection,
  public = list(
     initialize = function(pe, tm, max_features = NA, batch_size = 10) {
        super$initialize(id = "random_selection", pe = pe, tm = tm,
                         settings = list(max_features = checkmate::assert_numeric(max_features,
                                                                                  lower = 1,
                                                                                  upper = length(pe$task$feature_names)),
                                         batch_size = checkmate::assert_numeric(batch_size)))
     },

   get_result = function() {
      if(length(self$pe$bmr) > 1) {
         bmr = lapply(self$pe$bmr[1:length(self$pe$bmr)], function(bmr) self$pe$bmr[[1]]$combine(bmr))
      } else {
         bmr = self$pe$bmr
      }
      bmr_best = bmr[[length(bmr)]]$get_best(self$pe$task$measures[[1L]]$id)
      list(features = bmr_best$task$feature_names,
           performance = bmr_best$aggregated)
   }
  ),
  private = list(
   generate_states = function() {
     lapply(seq_len(self$settings$batch_size), function(i) {
       if(is.na(self$settings$max_features)) {
         return(rbinom(length(self$pe$task$feature_names), 1, 0.5))
       }
        x = Inf
        while (sum(x) >= self$settings$max_features) {
           x = rbinom(length(self$pe$task$feature_names), 1, 0.5)
        }
        return(x)
     }
     )
     }
   )
  )
