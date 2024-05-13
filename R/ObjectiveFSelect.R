#' @title Class for Feature Selection Objective
#'
#' @description
#' Stores the objective function that estimates the performance of feature subsets.
#' This class is usually constructed internally by the [FSelectInstanceBatchSingleCrit] / [FSelectInstanceBatchMultiCrit].
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_callbacks
#'
#' @export
ObjectiveFSelect = R6Class("ObjectiveFSelect",
  inherit = Objective,
  public = list(

    #' @field task ([mlr3::Task]).
    task = NULL,

    #' @field learner ([mlr3::Learner]).
    learner = NULL,

    #' @field resampling ([mlr3::Resampling]).
    resampling = NULL,

    #' @field measures (list of [mlr3::Measure]).
    measures = NULL,

    #' @field store_models (`logical(1)`).
    store_models = NULL,

    #' @field store_benchmark_result (`logical(1)`).
    store_benchmark_result = NULL,

    #' @field callbacks (List of [CallbackBatchFSelect]s).
    callbacks = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      check_values = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      callbacks = NULL
      ) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE), task = self$task)
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$store_models = assert_flag(store_models)
      self$store_benchmark_result = assert_flag(store_benchmark_result) || self$store_models
      self$callbacks = assert_callbacks(as_callbacks(callbacks))

      super$initialize(
        id = sprintf("%s_on_%s", self$learner$id, self$task$id),
        properties = "noisy",
        domain = task_to_domain(self$task),
        codomain = measures_to_codomain(self$measures),
        constants = ps(resampling = p_uty()),
        check_values = check_values)

      # set resamplings in constants
      resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      if (!resampling$is_instantiated) resampling$instantiate(task)
      self$resampling = resampling
      self$constants$values$resampling = list(resampling)
    }
  )
)
