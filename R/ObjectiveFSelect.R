#' @title ObjectiveFSelect
#'
#' @description
#' Stores the objective function that estimates the performance of feature
#' subsets. This class is usually constructed internally by by the
#' [TuningInstanceSingleCrit] / [TuningInstanceMultiCrit].
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_store_models
#' @template param_check_values
#'
#' @export
ObjectiveFSelect = R6Class("ObjectiveFSelect",
  inherit = Objective,

  #' @description
  #' Creates a new instance of this [R6][R6::R6Class] class.
  public = list(

    #' @field task ([mlr3::Task])
    task = NULL,

    #' @field learner ([mlr3::Learner])
    learner = NULL,

    #' @field resampling ([mlr3::Resampling])
    resampling = NULL,

    #' @field measures (list of [mlr3::Measure])
    measures = NULL,

    #' @field store_models (`logical(1)`).
    store_models = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures,
      store_models = FALSE, check_values = TRUE) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE),
        task = self$task)
      self$resampling = assert_resampling(as_resampling(resampling,
        clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE),
        task = self$task, learner = self$learner)
      self$store_models = assert_logical(store_models)
      if (!resampling$is_instantiated) {
        self$resampling$instantiate(self$task)
      }

      domain = ParamSet$new(map(self$task$feature_names,
        function(s) ParamLgl$new(id = s)))

      codomain = ParamSet$new(map(self$measures, function(s) {
          ParamDbl$new(id = s$id,
            tags = ifelse(s$minimize, "minimize", "maximize"))
        }))

      super$initialize(id = sprintf("%s_on_%s", self$learner$id, self$task$id),
        domain = domain, codomain = codomain, check_values = check_values)
    }
  ),

  private = list(

    .eval_many = function(xss) {
      tasks = map(xss, function(x) {
        state = self$task$feature_names[unlist(x)]
        tsk = self$task$clone(deep = TRUE)
        tsk$select(state)
        return(tsk)
      })

      design = benchmark_grid(tasks, self$learner, self$resampling)
      bmr = benchmark(design, store_models = self$store_models)
      rr = map(seq(bmr$n_resample_results), function(x) bmr$resample_result(x))
      aggr = bmr$aggregate(self$measures)
      y = map_chr(self$measures, function(s) s$id)

      cbind(aggr[, y, with = FALSE], resample_result = rr)
    }
  )
)
