#' @title Multi-Criteria Feature Selection with Rush
#'
#' @include FSelectInstanceAsyncSingleCrit.R ArchiveAsyncFSelect.R
#'
#' @description
#' The `FSelectInstanceAsyncMultiCrit` specifies a feature selection problem for a [FSelectorAsync].
#' The function [fsi_async()] creates a [FSelectInstanceAsyncMultiCrit] and the function [fselect()] creates an instance internally.
#'
#' @inheritSection FSelectInstanceBatchSingleCrit Default Measures
#' @inheritSection ArchiveAsyncFSelect Analysis
#' @inheritSection FSelectInstanceBatchSingleCrit Resources
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_rush
#'
#' @template param_xdt
#' @template param_extra
#'
#' @export
FSelectInstanceAsyncMultiCrit = R6Class("FSelectInstanceAsyncMultiCrit",
  inherit = OptimInstanceAsyncMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      terminator,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      rush = NULL
      ) {
      require_namespaces("rush")
      learner = assert_learner(as_learner(learner, clone = TRUE))
      callbacks = assert_async_fselect_callbacks(as_callbacks(callbacks))

      if (is.null(rush)) rush = rush::rsh()

      # create codomain from measures
      measures = assert_measures(as_measures(measures, task_type = task$task_type), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # create search space from task
      search_space = task_to_domain(task)

      archive = ArchiveAsyncFSelect$new(
        search_space = search_space,
        codomain = codomain,
        rush = rush
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
        objective = objective,
        search_space = search_space,
        terminator = terminator,
        callbacks = callbacks,
        archive = archive,
        rush = rush)
    },

    #' @description
    #' The [FSelectorAsync] object writes the best found points and estimated performance values here (probably the Pareto set / front).
    #' For internal use.
    #'
    #' @param ydt (`numeric()`)\cr
    #' Optimal outcomes, e.g. the Pareto front.
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, ydt, extra = NULL, ...) {
      # add feature names to result for easy task subsetting
      feature_names = self$objective$task$feature_names
      features = map(seq_len(nrow(xdt)), function(i) {
        feature_names[as.logical(xdt[i, feature_names, with = FALSE])]
      })
      set(xdt, j = "features", value = list(features))
      set(xdt, j = "n_features", value = map_int(features, length))

      # assign for callbacks
      private$.result_xdt = xdt
      private$.result_ydt = ydt
      private$.result_extra = extra

      call_back("on_fselect_result_begin", self$objective$callbacks, self$objective$context)

      super$assign_result(private$.result_xdt, private$.result_ydt)
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
