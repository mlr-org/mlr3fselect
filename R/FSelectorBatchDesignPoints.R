#' @title Feature Selection with Design Points
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_design_points
#'
#' @description
#' Feature selection using user-defined feature sets.
#'
#' @details
#' The feature sets are evaluated in order as given.
#'
#' The feature selection terminates itself when all feature sets are evaluated.
#' It is not necessary to set a termination criterion.
#'
#' @templateVar id design_points
#' @template section_dictionary_fselectors
#'
#' @inheritSection bbotk::OptimizerBatchDesignPoints Parameters
#'
#' @family FSelector
#' @export
#' @examples
#' # Feature Selection
#' \donttest{
#'
#' # retrieve task and load learner
#' task = tsk("pima")
#' learner = lrn("classif.rpart")
#'
#' # create design
#' design = mlr3misc::rowwise_table(
#'   ~age, ~glucose, ~insulin, ~mass, ~pedigree, ~pregnant, ~pressure, ~triceps,
#'   TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       FALSE,    TRUE,
#'   TRUE, TRUE,     FALSE,    TRUE,  FALSE,     TRUE,       FALSE,    FALSE,
#'   TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       FALSE,    FALSE,
#'   TRUE, FALSE,    TRUE,     TRUE,  FALSE,     TRUE,       TRUE,     TRUE
#' )
#'
#' # run feature selection on the Pima Indians diabetes data set
#' instance = fselect(
#'   fselector = fs("design_points", design = design),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce")
#' )
#'
#' # best performing feature set
#' instance$result
#'
#' # all evaluated feature sets
#' as.data.table(instance$archive)
#'
#' # subset the task and fit the final model
#' task$select(instance$result_feature_set)
#' learner$train(task)
#' }
FSelectorBatchDesignPoints = R6Class("FSelectorBatchDesignPoints",
  inherit = FSelectorBatchFromOptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerBatchDesignPoints$new(),
        man = "mlr3fselect::mlr_fselectors_design_points"
      )
    }
  )
)

mlr_fselectors$add("design_points", FSelectorBatchDesignPoints)
