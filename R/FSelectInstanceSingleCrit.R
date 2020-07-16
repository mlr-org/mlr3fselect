#' @title Single Criterion Feature Selection Instance
#'
#' @description
#' Specifies a general feature selection scenario, including objective function
#' and archive for feature selection algorithms to act upon. This class stores
#' an [ObjectiveFSelect] object that encodes the black box objective function
#' which an [FSelector] has to optimize. It allows the basic operations of
#' querying the objective at feature subsets (`$eval_batch()`), storing the
#' evaluations in the internal [Archive] and accessing the final result
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
#' @export
#' @examples
#' library(mlr3)
#'
#' # Objects required to define the performance evaluator
#' task = tsk("iris")
#' measures = msrs(c("classif.ce"))
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#' terminator = trm("evals", n_evals = 8)
#'
#' inst = FSelectInstanceSingleCrit$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   terminator = terminator
#' )
FSelectInstanceSingleCrit = R6Class("FSelectInstanceSingleCrit",
  inherit = OptimInstanceSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task]).
    #' @param learner ([mlr3::Learner]).
    #' @param resampling ([mlr3::Resampling])\cr
    #' Note that uninstantiated resamplings are instantiated during construction
    #' so that all configurations are evaluated on the same data splits.
    #'
    #' @param measure ([mlr3::Measure])\cr
    #' Measure to optimize.
    #'
    #' @param terminator ([Terminator]).
    #' @param store_models (`logical(1)`).
    #' @param check_values (`logical(1)`)\cr
    #' Should feature sets before the evaluation and the results be checked for
    #' validity?
    initialize = function(task, learner, resampling, measure, terminator,
      store_models = FALSE, check_values = TRUE) {
      obj = ObjectiveFSelect$new(task = task, learner = learner,
        resampling = resampling, measures = measure,
        store_models = store_models, check_values = check_values)
      super$initialize(obj, obj$domain, terminator)
    },

    #' @description
    #' The [FSelector] object writes the best found feature subset
    #' and estimated performance value here. For internal use.
    #'
    #' @param xdt (`data.table`)\cr
    #' x values as `data.table` with one row. Contains the value in the *search
    #' space* of the [FSelectInstance]. Can contain additional columns for extra
    #' information.
    #' @param y (`numeric(1)`)\cr
    #'   Optimal outcome.
    assign_result = function(xdt, y) {
      # Add feature names to result for easy task subsetting
      features = list(self$objective$task$feature_names[as.logical(xdt)])
      xdt[, features := list(features)]
      super$assign_result(xdt, y)
    }
  )
)
