#' @title Feature Selection via Design Points
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_design_points
#'
#' @description
#' Design points uses feature sets specified by the user.
#'
#' The feature sets are evaluated in order as given.
#' The feature selection terminates itself when all feature sets are evaluated.
#' It is not necessary to set a termination criterion.
#'
#' @templateVar id design_points
#' @template section_dictionary_fselectors
#'
#' @inheritSection bbotk::OptimizerDesignPoints Parameters
#'
#' @export
#' @examples
#' library(mlr3misc)
#'
#' # retrieve task
#' task = tsk("pima")
#'
#' # load learner
#' learner = lrn("classif.rpart")
#'
#' # create design
#' design = rowwise_table(
#'   ~age, ~glucose, ~insulin, ~mass, ~pedigree, ~pregnant, ~pressure, ~triceps,
#'   TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       FALSE,    TRUE,
#'   TRUE, TRUE,     FALSE,    TRUE,  FALSE,     TRUE,       FALSE,    FALSE,
#'   TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       FALSE,    FALSE,
#'   TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       TRUE,     TRUE
#' )
#'
#' \donttest{
#' # feature selection on the pima indians diabetes data set
#' instance = fselect(
#'   method = "design_points",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   design = design
#' )
#'
#' # best performing feature subset
#' instance$result
#'
#' # all evaluated feature subsets
#' as.data.table(instance$archive)
#'
#' # subset the task and fit the final model
#' task$select(instance$result_feature_set)
#' learner$train(task)
#' }
FSelectorDesignPoints = R6Class("FSelectorDesignPoints",
  inherit = FSelectorFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerDesignPoints$new(),
        man = "mlr3fselect::mlr_fselectors_design_points"
      )
    }
  )
)

mlr_fselectors$add("design_points", FSelectorDesignPoints)
