#' @title Class for Feature Selection Objective
#'
#' @description
#' Stores the objective function that estimates the performance of feature subsets.
#' This class is usually constructed internally by the [FSelectInstanceAsyncSingleCrit] or [FSelectInstanceAsyncMultiCrit].
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
ObjectiveFSelectAsync = R6Class("ObjectiveFSelectAsync",
  inherit = ObjectiveFSelect,
  public = list(
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

      private$.aggregator = if (all(c("requires_task", "requires_learner", "requires_model", "requires_train_set") %nin% self$measures$properties) && self$codomain$length == 1) async_aggregator_fast else async_aggregator_default
    }
  ),
  private = list(
    .eval = function(xs, resampling) {
      lg$debug("Evaluating feature subset %s", as_short_string(xs))

      # restore features
      all_features = self$task$feature_names
      on.exit(self$task$set_col_roles(all_features, "feature"))

      # select features
      private$.xs = xs
      call_back("on_eval_after_xs", self$callbacks, self$context)
      self$task$select(names(private$.xs)[as.logical(private$.xs)])

      lg$debug("Resampling feature subset")

      # resample feature subset
      private$.resample_result = resample(self$task, self$learner, self$resampling, store_models = self$store_models, clone = character(0), callbacks = self$callbacks)
      call_back("on_eval_after_resample", self$callbacks, self$context)

      lg$debug("Aggregating performance")

      # aggregate performance using the appropriate aggregator
      private$.aggregated_performance = as.list(private$.aggregator(private$.resample_result, self$measures))

      lg$debug("Aggregated performance %s", as_short_string(private$.aggregated_performance))

      # add runtime, errors and warnings
      warnings = sum(map_int(get_private(private$.resample_result)$.data$learner_states(), function(s) sum(s$log$class == "warning")))
      errors = sum(map_int(get_private(private$.resample_result)$.data$learner_states(), function(s) sum(s$log$class == "error")))
      runtime_learners = extract_runtime(private$.resample_result)

      private$.aggregated_performance = c(private$.aggregated_performance, list(runtime_learners = runtime_learners, warnings = warnings, errors = errors))

      # add benchmark result and models
      if (self$store_benchmark_result) {
        lg$debug("Storing resample result")
        private$.aggregated_performance = c(private$.aggregated_performance, list(resample_result = list(private$.resample_result)))
      }

      call_back("on_eval_before_archive", self$callbacks, self$context)
      private$.aggregated_performance
    },

    .xs = NULL,
    .resample_result = NULL,
    .aggregated_performance = NULL,
    .aggregator = NULL
  )
)

async_aggregator_default = function(resample_result, measures) {
  resample_result$aggregate(measures)
}

async_aggregator_fast = function(resample_result, measures) {
  mlr3::faggregate(resample_result, measures[[1]])
}
