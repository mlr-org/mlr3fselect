#' @title Single Criterion Feature Selection Instance
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
#' @template param_measure
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_xdt
#'
#' @export
#' @examples
#' library(mlr3)
#' library(data.table)
#'
#' # Objects required to define the objective function
#' task = tsk("iris")
#' measure = msr("classif.ce")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#'
#' # Create instance
#' terminator = trm("evals", n_evals = 8)
#' inst = FSelectInstanceSingleCrit$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measure = measure,
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
#' as.data.table(inst$archive)
FSelectInstanceSingleCrit = R6Class("FSelectInstanceSingleCrit",
  inherit = OptimInstanceSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measure, terminator,
      store_models = FALSE, check_values = TRUE, store_benchmark_result = TRUE) {
      obj = ObjectiveFSelect$new(task = task, learner = learner,
        resampling = resampling, measures = measure,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models, check_values = check_values)
      super$initialize(obj, obj$domain, terminator)

      self$archive = ArchiveFSelect$new(search_space = self$objective$domain,
        codomain = self$objective$codomain, check_values = check_values,
        store_x_domain = FALSE)
      self$objective$archive = self$archive

      private$.objective_function = objective_function
      private$.objective_multiplicator =
        ifelse(self$objective$codomain$tags == "minimize", 1, -1)
    },

    #' @description
    #' The [FSelector] writes the best found feature subset
    #' and estimated performance value here. For internal use.
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    assign_result = function(xdt, y) {
      # Add feature names to result for easy task subsetting
      features = list(self$objective$task$feature_names[as.logical(xdt)])
      xdt[, features := list(features)]
      super$assign_result(xdt, y)
    }
  ),

  active = list(
    #' @field result_feature_set (`character()`)\cr
    #' Feature set for task subsetting.
    result_feature_set = function() {
      unlist(self$result$features)
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
