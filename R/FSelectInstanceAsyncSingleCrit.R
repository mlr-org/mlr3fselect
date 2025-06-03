#' @title Single Criterion Feature Selection with Rush
#'
#' @description
#' The `FSelectInstanceAsyncSingleCrit` specifies a feature selection problem for a [FSelectorAsync].
#' The function [fsi_async()] creates a [FSelectInstanceAsyncSingleCrit] and the function [fselect()] creates an instance internally.
#'
#' @details
#' The instance contains an [ObjectiveFSelectAsync] object that encodes the black box objective function a [FSelector] has to optimize.
#' The instance allows the basic operations of querying the objective at design points (`$eval_async()`).
#' This operation is usually done by the [FSelector].
#' Feature subsets are asynchronously sent to workers and evaluated by calling [mlr3::resample()].
#' The evaluated feature subsets are stored in the [ArchiveAsyncFSelect] (`$archive`).
#' Before a batch is evaluated, the [bbotk::Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#' The fselector is also supposed to store its final result, consisting of a selected feature subset and associated estimated performance values, by calling the method `instance$assign_result`.
#'
#' @inheritSection FSelectInstanceBatchSingleCrit Default Measures
#' @inheritSection ArchiveAsyncFSelect Analysis
#' @inheritSection FSelectInstanceBatchSingleCrit Resources
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_rush
#' @template param_ties_method
#'
#' @template param_xdt
#' @template param_extra
#'
#' @export
FSelectInstanceAsyncSingleCrit = R6Class("FSelectInstanceAsyncSingleCrit",
  inherit = OptimInstanceAsyncSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measure = NULL,
      terminator,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      ties_method = "least_features",
      rush = NULL
      ) {
      require_namespaces("rush")
      learner = assert_learner(as_learner(learner, clone = TRUE))
      callbacks = assert_async_fselect_callbacks(as_callbacks(callbacks))

      if (is.null(rush)) rush = rush::rsh()

      # create codomain from measure
      measures = assert_measures(as_measures(measure, task_type = task$task_type), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # create search space from task
      search_space = task_to_domain(task)

      archive = ArchiveAsyncFSelect$new(
        search_space = search_space,
        codomain = codomain,
        rush = rush,
        ties_method = ties_method
      )

      objective = ObjectiveFSelectAsync$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        callbacks = callbacks)

      super$initialize(
        objective,
        search_space,
        terminator,
        callbacks = callbacks,
        rush = rush,
        archive = archive)
    },

    #' @description
    #' The [FSelectorAsync] object writes the best found point and estimated performance value here.
    #' For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, y, extra = NULL, ...) {
      # add feature names to result for easy task subsetting
      feature_names = self$objective$task$feature_names
      features = list(feature_names[as.logical(xdt[, feature_names, with = FALSE])])
      set(xdt, j = "features", value = list(features))
      set(xdt, j = "n_features", value = length(features[[1L]]))

      # assign for callbacks
      private$.result_xdt = xdt
      private$.result_y = y
      private$.result_extra = extra

      call_back("on_fselect_result_begin", self$objective$callbacks, self$objective$context)

      super$assign_result(private$.result_xdt, private$.result_y)
      if (!is.null(private$.result$x_domain)) set(private$.result, j = "x_domain", value = NULL)
    }
  ),

  private = list(
    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextAsyncFSelect$new(self, optimizer)
      self$objective$context = context
    }
  )
)
