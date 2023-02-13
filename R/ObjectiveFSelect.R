#' @title Class for Feature Selection Objective
#'
#' @description
#' Stores the objective function that estimates the performance of feature subsets.
#' This class is usually constructed internally by by the [FSelectInstanceSingleCrit] / [FSelectInstanceMultiCrit].
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

    #' @field archive ([ArchiveFSelect]).
    archive = NULL,

    #' @field callbacks (List of [CallbackFSelect]s).
    callbacks = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param archive ([ArchiveFSelect])\cr
    #'   Reference to the archive of [FSelectInstanceSingleCrit] | [FSelectInstanceMultiCrit].
    #'   If `NULL` (default), benchmark result and models cannot be stored.
    initialize = function(task, learner, resampling, measures, check_values = TRUE, store_benchmark_result = TRUE, store_models = FALSE, archive = NULL, callbacks = list()) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE), task = self$task)
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)

      self$archive = assert_r6(archive, "ArchiveFSelect", null.ok = TRUE)
      if (is.null(self$archive)) store_benchmark_result = store_models = FALSE
      self$store_models = assert_flag(store_models)
      self$store_benchmark_result = assert_flag(store_benchmark_result) || self$store_models
      self$callbacks = assert_callbacks(as_callbacks(callbacks))

      super$initialize(
        id = sprintf("%s_on_%s", self$learner$id, self$task$id),
        domain = task_to_domain(self$task),
        codomain = measures_to_codomain(self$measures),
        check_values = check_values,
        constants = ps(resampling = p_uty()))

      # set resamplings in constants
      resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      if (!resampling$is_instantiated) resampling$instantiate(task)
      self$resampling = resampling
      self$constants$values$resampling = list(resampling)
    }
  ),

  private = list(
    .eval_many = function(xss, resampling) {
      context = ContextEval$new(self)
      private$.xss = xss

      tasks = map(private$.xss, function(x) {
        state = self$task$feature_names[unlist(x)]
        task = self$task$clone()
        task$select(state)
        task
      })

      # benchmark feature subsets
      private$.design = data.table(task = tasks, learner = list(self$learner), resampling = resampling)
      call_back("on_eval_after_design", self$callbacks, context)

      # learner is already cloned, task is internally cloned by PipeOpSelect, and resampling is not changed
      private$.benchmark_result = benchmark(private$.design, store_models = self$store_models || private$.model_required, clone = character())
      call_back("on_eval_after_benchmark", self$callbacks, context)

      # aggregate performance scores
      private$.aggregated_performance = private$.benchmark_result$aggregate(self$measures, conditions = TRUE)[, c(self$codomain$target_ids, "warnings", "errors"), with = FALSE]

      # add runtime to evaluations
      time = map_dbl(private$.benchmark_result$resample_results$resample_result, function(rr) {
        sum(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) state$train_time + state$predict_time))
      })
      set(private$.aggregated_performance, j = "runtime_learners", value = time)

      # store benchmark result in archive
      if (self$store_benchmark_result) {
        self$archive$benchmark_result$combine(private$.benchmark_result)
        set(private$.aggregated_performance, j = "uhash", value = private$.benchmark_result$uhashes)
      }
      private$.aggregated_performance
    },

    .xss = NULL,
    .design = NULL,
    .benchmark_result = NULL,
    .aggregated_performance = NULL,
    .model_required = FALSE
  )
)
