#' @title Abstract PerformanceEvaluator Class
#'
#' @description
#' `PerformanceEvaluator` class that implements the performance evaluation on a set of feature combinations. A pe is an object that stores all informations that are necesarry to conduct a feature selection (`mlr3::Task`, `mlr3::Learner`, `mlr3::Resampling`).
#'
#' @section Usage:
#' ```
#' # Construction
#' pe = PerformanceEvaluator$new()
#'
#' # Public members
#' pe$task
#' pe$learner
#' pe$resampling
#' pe$bmr
#'
#' # Public methods
#' pe$eval_states(states)
#' pe$get_best()
#' ```
#'
#' @section Arguments:
#' * `task` (`mlr3::Task`):
#'   The task that we want to evaluate.
#' * `learner` (`mlr3::Learner`):
#'   The learner that we want to evaluate.
#' * `resampling` (`mlr3::Resampling`):
#'   The Resampling method that is used to evaluate the learner.
#'
#' @section Details:
#' * `$new()` creates a new object of class [PerformanceEvaluator].
#' * `$task` (`mlr3::Task`) the task for which the feature selection should be conducted.
#' * `$learner` (`mlr3::Learner`) the algorithm for which the feature selection should be conducted.
#' * `$resampling` (`mlr3::Resampling`) strategy to evaluate a feature combination
#' * `$bmr` (`list`) of (`mlr3::BenchmarkResult`) objects. Each entry corresponds to one batch or step depending one the used feature selection method.
#' * `$eval_states(states)` evaluates the feature combinations `states`.
#' * `$get_best()` returns selected features with the best performance of each entry in `$bmr`.
#'
#' @name PerformanceEvaluator
#' @keywords internal
#' @family PerformanceEvaluator
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' pe = PerformanceEvaluator$new(task, learner, resampling)
NULL

#' @export
PerformanceEvaluator = R6Class("PerformanceEvaluator",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    bmr = list(),
    states = list(),

    initialize = function(task, learner, resampling) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
    },

    eval_states = function(states) {
      self$states[[length(self$states)+1]] <- states
      # For each state, clone task and set feature subset
      task_list <- list()
      for(state in states) {
        task = self$task$clone()
        task$select(state)
        task_list[[length(task_list)+1]] <- task
      }

      new_bmr = benchmark(data.table::data.table(task = task_list,
                                     learner = list(self$learner),
                                     resampling = list(self$resampling)))

      self$bmr[[length(self$bmr)+1]] <- new_bmr
    },

    get_best = function() {
      lapply(self$bmr, function(x) {
        rr = x$get_best(self$task$measures[[1L]]$id)
        list(features = rr$task$feature_names,
             performance = mean(rr$performance(self$task$measures[[1L]]$id)))
      })
    }
  )
)
