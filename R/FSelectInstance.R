#' @title FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario, including objective function
#' and archive for Optimizers to act upon. This class stores an [ObjectiveFSelect]
#' object that encodes the black box objective function which an [Optimizer] has to
#' optimize. It allows the basic operations of querying the objective
#' at feature subsets (`$eval_batch()`), stroring the evaluations in the internal
#' [Archive] and accessing the final result (`$result`).
#'
#' Evaluations of feature subsets are performed in batches by
#' calling [mlr3::benchmark()] internally. Before a batch is evaluated, the
#' [Terminator] is queried for the remaining budget. If the available budget is
#' exhausted, an exception is raised, and no further evaluations can be
#' performed from this point on.
#'
#' The Optimizer object is also supposed to store its final result, consisting
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
#' terminator = term("evals", n_evals = 8)
#'
#' inst = FSelectInstance$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   terminator = terminator
#' )
FSelectInstance = R6Class("FSelectInstance",
  inherit = OptimInstance,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])
    #' @param learner ([mlr3::Learner])
    #' @param resampling ([mlr3::Resampling])\cr
    #' Note that uninstantiated resamplings are instantiated during construction
    #' so that all configurations are evaluated on the same data splits.
    #' @param measure ([mlr3::Measure])\cr
    #' Measure to optimize.
    #' @param terminator ([Terminator])
    #' @param store_models (`logical(1)`)
    initialize = function(task, learner, resampling, measure, terminator,
      store_models = FALSE) {
        obj = ObjectiveFSelect$new(task = task, learner = learner,
          resampling = resampling, measures = measure, store_models = store_models)
        super$initialize(obj, obj$domain, terminator)
    }
  )
)
