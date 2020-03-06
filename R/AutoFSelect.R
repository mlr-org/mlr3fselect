#' AutoFeselect
#'
#' @description
#' The `AutoFSelect` is a [mlr3::Learner] which automatically selects the optimal feature set and
#' fits the model on the training data with the optimal feature set.
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
#' fs = fs("exhaustive")
#' afs = AutoFSelect$new(learner, resampling, measures, terminator, fs)
#' afs$store_fselect_instance = TRUE
#'
#' afs$train(task)
#' afs$model
#' afs$learner
AutoFSelect = R6Class("AutoFSelect", inherit = Learner,
  public = list(
    #' @field instance_args `list()`
    #' @field fselect [FSelect]
    #' @field store_fselect_instance `logical(1)`
    instance_args = NULL,
    fselect = NULL,
    store_fselect_instance = TRUE,

    #' @description
    #' Create new `AutoFSelect` object.
    #' @param learner [mlr3::Learner]
    #' @param resampling [mlr3::Resampling]
    #' @param measures list of [mlr3::Measure]
    #' @param terminator [Terminator]
    #' @param bm_args named `list()` Further arguments for [mlr3::benchmark()].
    #' @param fselect [FSelect]
    #' @return `AutoFSelect`
    initialize = function(learner, resampling, measures, terminator, fselect, bm_args = list()) {

      ia = list()
      ia$learner = assert_learner(learner)$clone()
      ia$resampling = assert_resampling(resampling, instantiated = FALSE)$clone()
      ia$measures = assert_measures(as_measures(measures), learner = learner)
      ia$terminator = assert_terminator(terminator)$clone()
      ia$bm_args = assert_list(bm_args, names = "unique")
      self$instance_args = ia
      self$fselect = assert_r6(fselect, "FSelect")$clone()

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
    },

    #' @description
    #' Selects the optimal feature set, applies the feature set to the task
    #' and fits the model on the training data.
    #' If `store_fselect_instance` is `TRUE`, the [mlr3::Learner] with the [FSelectInstance]
    #' is returned, otherwise just the [mlr3::Learner]. For internal use.
    #' @param task [mlr3::Task]
    #' @return list([mlr3::Learner]) or list([mlr3::Learner], [FSelectInstance])
    train_internal = function(task) {

      ia = self$instance_args
      ia$task = task$clone()
      instance = invoke(FSelectInstance$new, .args = ia)
      self$fselect$select(instance)

      ia$task$select(instance$result$feat)
      learner = ia$learner
      learner$train(ia$task)

      result_model = list()
      result_model$learner = learner
      if (isTRUE(self$store_fselect_instance)) {
        result_model$fselect_instance = instance
      }
      return(result_model)
    },

    #' @description
    #' Creates a new [mlr3::Prediction] based on the learner fitted
    #' on the training data with the optimal feature subset.
    #' For internal use.
    #' @param task [mlr3::Task]
    #' @return [mlr3::Prediction]
    predict_internal = function(task) {
      self$model$learner$predict(task)
    },

    #' @description
    #' Returns a table of contained resample results with corresponding feature sets.
    #' @return [data.table::data.table]
    archive = function() self$fselect_instance$archive()
  ),

  active = list(

    #' @field learner [mlr3::Learner]
    learner = function() {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model)) {
        self$instance_args$learner
      } else {
        self$model$learner
      }
    },
    #' @field fselect_instance [FSelectInstance]
    fselect_instance = function() self$model$fselect_instance,

    #' @field fselect_result `list()` Result of the feature selection i.e. the optimal feature set and its estimated performances.
    fselect_result = function() self$fselect_instance$result
  )
)
