#' @title PerformanceEvaluator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Evaluates feature sets with [mlr3::Benchmark] genereated by [FeatureSelection].
#'
#' @section Construction:
#' ```
#' # Construction
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' ```
#'
#' * `task` :: [mlr3::Task]\cr
#'
#' * `learner` :: [mlr3::Learner]\cr
#'
#' * `resampling` :: [mlr3::Resampling]\cr
#'
#' @section Fields:
#' * `task` :: [mlr3::Task]\cr Stores the task.
#' * `learner` :: [mlr3::Learner]\cr Stores the learner.
#' * `resampling` :: [mlr3::Resampling]\cr Stores the resampling.
#' * `bmr` :: list of [mlr3::BenchmarkResult]\cr Stores the benchmark results.
#'
#' @section Methods:
#' * `eval_states(states)`\cr
#'   list of `integer()` -> `self`\cr
#'   Evaluates feature sets.
#'
#' @family PerformanceEvaluator
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' @export
PerformanceEvaluator = R6Class("PerformanceEvaluator",
  public = list(
    task = NULL,
    learner = NULL,
    resampling = NULL,
    bmr = list(),

    initialize = function(task, learner, resampling) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$resampling = mlr3::assert_resampling(resampling)
    },

    eval_states = function(states) {
      # For each state, clone task and set feature subset
      task_list <- list()
      for (state in states) {
        task = self$task$clone()
        task$select(state)
        task_list[[length(task_list) + 1]] <- task
      }

      # Evaluate
      new_bmr = benchmark(data.table::data.table(
        task = task_list,
        learner = list(self$learner),
        resampling = list(self$resampling)))
      self$bmr[[length(self$bmr) + 1]] <- new_bmr
    }
  )
)
