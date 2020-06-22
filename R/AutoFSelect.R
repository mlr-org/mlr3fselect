#' @title AutoFeselect
#'
#' @description
#' The `AutoFSelect` is a [mlr3::Learner] which automatically selects the
#' optimal feature set and fits the model on the training data with the optimal
#' feature set.
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("holdout")
#' measures = msr("classif.ce")
#'
#' terminator = term("evals", n_evals = 15)
#' fs = opt("exhaustive")
#' afs = AutoFSelect$new(learner, resampling, measures, terminator, fs)
#' afs$store_fselect_instance = TRUE
#'
#' afs$train(task)
#' afs$model
#' afs$learner
AutoFSelect = R6Class("AutoFSelect", inherit = Learner,
  public = list(

    #' @field instance_args (`list()`)\cr
    #' All arguments from construction to create the [FSelectInstance].
    instance_args = NULL,

    #' @field fselect ([bbotk::Optimizer])\cr
    #' Stores the feature selection algorithm.
    fselect = NULL,

    #' @field store_fselect_instance (`logical(1)`)
    store_fselect_instance = TRUE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::Learner])\cr
    #' Learner to optimize the feature subset for, see [FSelectInstance].
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #' Resampling strategy during feature selection, see [FSelectInstance]. This
    #' [mlr3::Resampling] is meant to be the **inner** resampling, operating
    #' on the training set of an arbitrary outer resampling. For this reason
    #' it is not feasible to pass an instantiated [mlr3::Resampling] here.
    #'
    #' @param measure ([mlr3::Measure])\cr
    #' Performance measure to optimize.
    #'
    #' @param terminator ([Terminator])\cr
    #' When to stop feature selection, see [FSelectInstance].
    #'
    #' @param fselect ([bbotk::Optimizer])\cr
    #' Feature selection algorithm to run.
    #'
    #' @param bm_args (named `list()`)\cr
    #' Further arguments for [mlr3::benchmark()], see [FSelectInstance].
    initialize = function(learner, resampling, measure, terminator, fselect) {
      ia = list()
      ia$learner = assert_learner(learner)$clone(deep = TRUE)
      ia$resampling = assert_resampling(resampling,
        instantiated = FALSE)$clone()
      ia$measure = assert_measure(as_measure(measure), learner = learner)
      ia$terminator = assert_terminator(terminator)$clone()
      self$instance_args = ia
      self$fselect = assert_r6(fselect, "Optimizer")$clone()

      super$initialize(
        id = paste0(learner$id, ".fselect"),
        task_type = learner$task_type,
        packages = learner$packages,
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        param_set = learner$param_set,
        properties = learner$properties
      )

      self$predict_type = learner$predict_type
    }
  ),

  private = list(

    .train = function(task) {
      ia = self$instance_args
      ia$task = task$clone()
      instance = invoke(FSelectInstance$new, .args = ia)
      self$fselect$optimize(instance)

      feat = task$feature_names[as.logical(instance$result_x_search_space)]
      ia$task$select(feat)

      learner = ia$learner$clone(deep = TRUE)
      learner$train(ia$task)

      result_model = list()
      result_model$learner = learner
      if (isTRUE(self$store_fselect_instance)) {
        result_model$fselect_instance = instance
      }
      return(result_model)
    },

    .predict = function(task) {
      self$model$learner$predict(task)
    }
  ),

  active = list(

    #' @field archive ([Archive])\cr
    #' Returns FSelectInstance archive.
    archive = function() self$fselect_instance$archive,

    #' @field learner ([mlr3::Learner])\cr
    #' Trained learner.
    learner = function() {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model)) {
        self$instance_args$learner
      } else {
        self$model$learner
      }
    },
    #' @field fselect_instance ([FSelectInstance])\cr
    #' Internally created feature selection instance with all intermediate results.
    fselect_instance = function() self$model$fselect_instance,

    #' @field fselect_result (named `list()`)\cr
    #' Short-cut to `$result` from [FSelectInstance].
    fselect_result = function() self$fselect_instance$result
  )
)
