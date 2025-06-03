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

      # aggregate performance
      private$.aggregated_performance = as.list(private$.resample_result$aggregate(self$measures))

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
    .aggregated_performance = NULL
  )
)
