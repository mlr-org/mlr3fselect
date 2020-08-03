#' @title Multi Criterion Feature Selection Instance
#'
#' @description
#' Specifies a general feature selection scenario, including objective function
#' and archive for feature selection algorithms to act upon. This class stores
#' an [ObjectiveFSelect] object that encodes the black box objective function
#' which an [FSelector] has to optimize. It allows the basic operations of
#' querying the objective at feature subsets (`$eval_batch()`), storing the
#' evaluations in the internal [bbotk::Archive] and accessing the final result
#' (`$result`).
#'
#' Evaluations of feature subsets are performed in batches by calling
#' [mlr3::benchmark()] internally. Before a batch is evaluated, the
#' [bbotk::Terminator] is queried for the remaining budget. If the available
#' budget is exhausted, an exception is raised, and no further evaluations can
#' be performed from this point on.
#'
#' The [FSelector] is also supposed to store its final result, consisting
#' of a selected feature subset and associated estimated performance values, by
#' calling the method `instance$assign_result()`.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_resample_results
#' @template param_xdt
#'
#' @export
#' @examples
#' library(mlr3)
#' library(data.table)
#'
#' # Objects required to define the performance evaluator
#' task = tsk("iris")
#' measures = msrs(c("classif.ce", "classif.acc"))
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#' terminator = trm("evals", n_evals = 8)
#'
#' inst = FSelectInstanceMultiCrit$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   terminator = terminator
#' )
#'
#' # Try some feature subsets
#' xdt = data.table(
#'   Petal.Length = c(TRUE, FALSE),
#'   Petal.Width = c(FALSE, TRUE),
#'   Sepal.Length = c(TRUE, FALSE),
#'   Sepal.Width = c(FALSE, TRUE)
#' )
#'
#' inst$eval_batch(xdt)
#'
#' # Get archive data
#' inst$archive$data()
FSelectInstanceMultiCrit = R6Class("FSelectInstanceMultiCrit",
  inherit = OptimInstanceMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures, terminator,
      store_models = FALSE, check_values = TRUE, store_resample_results = TRUE) {
      obj = ObjectiveFSelect$new(task = task, learner = learner,
        resampling = resampling, measures = measures,
        store_models = store_models, check_values = check_values,
        store_resample_results = store_resample_results)
      super$initialize(obj, obj$domain, terminator)

      private$.objective_function = objective_function
      private$.objective_multiplicator =
        ifelse(self$objective$codomain$tags == "minimize", 1, -1)
    },

    #' @description
    #' The [FSelector] object writes the best found feature subsets
    #' and estimated performance values here. For internal use.
    #'
    #' @param ydt (`data.table::data.table()`)\cr
    #' Optimal outcomes, e.g. the Pareto front.
    assign_result = function(xdt, ydt) {
      # Add feature names to result for easy task subsetting
      features = map(transpose_list(xdt), function(x) {
        self$objective$task$feature_names[as.logical(x)]
      })
      xdt[, features := list(features)]
      super$assign_result(xdt, ydt)
    }
  ),

  active = list(
    #' @field result_feature_set (`list()` of `character()`)\cr
    #' Feature sets for task subsetting.
    result_feature_set = function() {
      map(self$result$features, function(x) {
        unlist(x)
      })
    }
  )
)

objective_function = function(x, inst, multiplicator) {
  xs = set_names(as.list(as.logical(x)), inst$search_space$ids())
  inst$search_space$assert(xs)
  xdt = as.data.table(xs)
  res = inst$eval_batch(xdt)
  y = as.numeric(res[, inst$objective$codomain$ids(), with = FALSE])
  y * multiplicator
}
