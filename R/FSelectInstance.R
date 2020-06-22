#' @title FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario, including objective function
#' and archive for Optimizers to act upon. This class stores an
#' [ObjectiveFSelect] object that encodes the black box objective function which
#' an [Optimizer] has to optimize. It allows the basic operations of querying
#' the objective at feature subsets (`$eval_batch()`), stroring the evaluations
#' in the internal [Archive] and accessing the final result (`$result`).
#'
#' Evaluations of feature subsets are performed in batches by calling
#' [mlr3::benchmark()] internally. Before a batch is evaluated, the [Terminator]
#' is queried for the remaining budget. If the available budget is exhausted, an
#' exception is raised, and no further evaluations can be performed from this
#' point on.
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
#' measure = msr("classif.ce")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#' terminator = term("evals", n_evals = 8)
#'
#' inst = FSelectInstance$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measure = measure,
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
    },

    #' @description
    #' Evaluates a single feature set encoded as 0/1, and returns a
    #' scalar objective value, where the return value is negated if the measure
    #' is maximized. Internally, `$eval_batch()` is called with a single row.
    #' This function serves as a objective function for optimizers of binary
    #' spaces.
    #'
    #' @param x (`integer()`)\cr
    #' 0/1 encoded feature set.
    #'
    #' @return Objective value as `numeric(1)`.
    objective_function = function(x) {
      assert_subset(x, choices = c(1,0))
      assert_integer(as.integer(x), len = self$search_space$length,
        any.missing = FALSE)
      xs = set_names(as.list(as.logical(x)), self$search_space$ids())
      self$search_space$assert(xs)
      xdt = as.data.table(xs)
      res = self$eval_batch(xdt)
      y = as.numeric(res[, self$objective$codomain$ids()[1], with=FALSE])
      if(self$objective$codomain$tags[[1]] == "minimize") y else -y
    }
  )
)
