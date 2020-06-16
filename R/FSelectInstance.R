#' @title FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario.
#'
#' Evaluations of feature sets are performed in batches. Before and after a
#' batch is evaluated, the [Terminator] is queried for the remaining budget. If
#' the available budget is exhausted, an exception is raised, and no further
#' evaluations can be performed from this point on.
#'
#' A list of measures can be passed to the instance, and they will always be all
#' evaluated. However, single-criteria tuners optimize only the first measure.
#'
#' The [FSelect] object is also supposed to store its final result, consisting
#' of a selected feature set and associated estimated performance values, by
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
#' instance = FSelectInstance$new(
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
    #' @param task [mlr3::Task]
    #' @param learner [mlr3::Learner]
    #' @param resampling [mlr3::Resampling]
    #' @param measures list of [mlr3::Measure]
    #' @param terminator [Terminator]
    #' @param store_models `logical(1)`
    initialize = function(task, learner, resampling, measures, terminator,
      store_models = FALSE) {
        obj = ObjectiveFSelect$new(task = task, learner = learner,
          resampling = resampling, measures = measures, store_models = store_models)
        super$initialize(obj, obj$domain, terminator)
    }
  )
)
