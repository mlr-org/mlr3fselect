#' @title FeatureSelectionSequential
#'
#' @description
#' FeatureSelection child class to conduct forward search.
#'
#' @section Usage:
#'  ```
#' fs = FeatureSelectionSequential$new()
#' ```
#' See [FeatureSelection] for a description of the interface.
#'
#' @section Arguments:
#' * `pe` (`[PerformanceEvaluator]`).
#' * `tm` (`[Terminator]`).
#'
#' @section Details:
#' `$new()` creates a new object of class [FeatureSelectionSequential].
#' `$get_result()` Returns selected features in each step.
#' The interface is described in [FeatureSelection].
#'
#' Each step is possibly executed in parallel via [mlr3::benchmark()]
#'
#' @name FeatureSelectionSequential
#' @family FeatureSelection
#' @examples
#' task = mlr3::mlr_tasks$get("pima")
#' measures = mlr3::mlr_measures$mget(c("classif.acc"))
#' task$measures = measures
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorPerformanceStep$new(threshold = 0.01)
#' fs = FeatureSelectionSequential$new(pe, tm)
#' fs$calculate()
#' fs$get_result()
NULL

#' @export
#' @include FeatureSelection.R

FeatureSelectionSequential = R6Class("FeatureSelectionSequential",
   inherit = FeatureSelection,
   public = list(
      initialize = function(pe, tm, max_features = NA) {
         if(is.na(max_features)) {
            max_features = length(pe$task$feature_names)
         }

         super$initialize(id = "forward_selection", pe = pe, tm = tm,
                          settings = list(max_features = checkmate::assert_numeric(max_features,
                                                                                   lower = 1,
                                                                                   upper = length(pe$task$feature_names))))

         self$state = rep(0, length(pe$task$feature_names))
     },

     get_result = function() {
        bmr = self$pe$bmr[[length(self$pe$bmr)]]$get_best(self$pe$task$measures[[1L]]$id)
        list(features = bmr$task$feature_names,
             performance = bmr$aggregated)
     },
     get_path = function() {
        lapply(self$pe$bmr, function(bmr) {
           bmr = bmr$get_best(self$pe$task$measures[[1L]]$id)
           list(features = bmr$task$feature_names,
                performance = bmr$aggregated)
        })
     }
   ),
   private = list(
      generate_states = function() {
         new_states = list()
         for (i in seq_along(self$state)) {
            if (self$state[i] == 0) {
            state = self$state
            state[i] = 1
            new_states[[length(new_states) + 1]] = state
            }
         }
         new_states
      },
      eval_states_terminator = function(states) {
         self$tm$update_start(self$pe)
         self$pe$eval_states(states)
         self$tm$update_end(self$pe)

         # Side-effect stop maximum features
         if(!self$tm$terminated) {
            self$tm$terminated = (length(states[[1]]) == self$settings$max_features)
         }
      }
   )
)
