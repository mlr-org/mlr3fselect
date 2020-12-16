#' @title Feature Selection via Design Points
#'
#' @description
#' `FSelectorDesignPoints` class that implements feature selection w.r.t. fixed
#' feature sets.  We simply search over a set of feature subsets fully specified
#' by the user. The feature sets are evaluated in order as given.
#'
#' In order to support general termination criteria and parallelization, we
#' evaluate feature sets in a batch-fashion of size `batch_size`. Larger
#' batches mean we can parallelize more, smaller batches imply a more
#' fine-grained checking of termination criteria.
#'
#' @templateVar id design_points
#' @template section_dictionary_fselectors
#'
#' @inheritSection bbotk::OptimizerDesignPoints Parameters
#'
#' @export
#' @examples
#' library(mlr3)
#' library(data.table)
#'
#' terminator = trm("evals", n_evals = 10)
#'
#' instance = FSelectInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' design = data.table(Petal.Length = c(TRUE, FALSE),
#'   Petal.Width = c(TRUE, FALSE),
#'   Sepal.Length = c(FALSE, TRUE),
#'   Sepal.Width = c(FALSE, TRUE))
#'
#' fselector = fs("design_points", design = design)
#' \donttest{
#' # Modifies the instance by reference
#' fselector$optimize(instance)
#'
#' # Returns best scoring evaluation
#' instance$result
#'
#' # Allows access of data.table of full path of all evaluations
#' as.data.table(instance$archive)}
FSelectorDesignPoints = R6Class("FSelectorDesignPoints",
  inherit = FSelectorFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerDesignPoints$new()
      )
    }
  )
)

mlr_fselectors$add("design_points", FSelectorDesignPoints)
