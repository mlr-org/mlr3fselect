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
#' @template param_aggregate_fast
#'
#' @export
ObjectiveFSelectBatch = R6Class("ObjectiveFSelectBatch",
  inherit = ObjectiveFSelect,
  public = list(

    #' @field archive ([ArchiveBatchFSelect]).
    archive = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param archive ([ArchiveBatchFSelect])\cr
    #'   Reference to the archive of [FSelectInstanceBatchSingleCrit] | [FSelectInstanceBatchMultiCrit].
    #'   If `NULL` (default), benchmark result and models cannot be stored.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      check_values = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      archive = NULL,
      callbacks = NULL,
      aggregate_fast = FALSE
      ) {
      self$archive = assert_r6(archive, "ArchiveBatchFSelect", null.ok = TRUE)
      if (is.null(self$archive)) store_benchmark_result = store_models = FALSE
      assert_flag(aggregate_fast)

      super$initialize(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks
      )

      if (aggregate_fast && any(c("requires_task", "requires_learner", "requires_model", "requires_train_set") %in% self$measures$properties)) {
        stopf("Fast aggregation is only supported for measures that do not require task, learner, model or train set")
      }

      private$.aggregator = if (aggregate_fast) aggregator_fast else aggregator_default
    }
  ),

  private = list(
    .eval_many = function(xss, resampling) {
      private$.xss = xss

      tasks = map(private$.xss, function(x) {
        state = self$task$feature_names[unlist(x)]
        task = self$task$clone()
        always_included = task$col_roles$always_included
        task$set_col_roles(always_included, "feature")
        task$select(c(state, always_included))
        task
      })

      # benchmark feature subsets
      private$.design = data.table(task = tasks, learner = list(self$learner), resampling = resampling)
      call_back("on_eval_after_design", self$callbacks, self$context)

      lg$debug("Resampling feature subsets")

      # learner is already cloned, task is internally cloned by PipeOpSelect, and resampling is not changed
      private$.benchmark_result = benchmark(private$.design, store_models = self$store_models || private$.model_required, clone = character())
      call_back("on_eval_after_benchmark", self$callbacks, self$context)

      lg$debug("Aggregating performance")

      # aggregate performance scores
      browser()
      private$.aggregated_performance = private$.aggregator(private$.benchmark_result, self$measures, self$codomain)

      lg$debug("Aggregated performance %s", as_short_string(private$.aggregated_performance))

      # add runtime to evaluations
      time = map_dbl(private$.benchmark_result$resample_results$resample_result, function(rr) {
        sum(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) state$train_time + state$predict_time))
      })
      set(private$.aggregated_performance, j = "runtime_learners", value = time)

      # store benchmark result in archive
      if (self$store_benchmark_result) {
        lg$debug("Storing resample result")
        self$archive$benchmark_result$combine(private$.benchmark_result)
        set(private$.aggregated_performance, j = "uhash", value = private$.benchmark_result$uhashes)
      }

      call_back("on_eval_before_archive", self$callbacks, self$context)

      private$.aggregated_performance
    },

    .xss = NULL,
    .design = NULL,
    .benchmark_result = NULL,
    .aggregated_performance = NULL,
    .model_required = FALSE,
    .aggregator = NULL
  )
)

aggregator_default = function(benchmark_result, measures, codomain) {
  benchmark_result$aggregate(measures, conditions = TRUE)[, c(codomain$target_ids, "warnings", "errors"), with = FALSE]
}

aggregator_fast = function(benchmark_result, measures, codomain) {
  mlr3::faggregate(benchmark_result, measures[[1]])
}