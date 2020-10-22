#' @title AutoFSelector
#'
#' @description
#' The `AutoFSelector` is a [mlr3::Learner] which wraps another [mlr3::Learner]
#' and performs the following steps during `$train()`:
#'
#' 1. The wrapped (inner) learner is trained on the feature subsets via
#'    resampling. The feature selection can be specified by providing a
#'    [FSelector], a [bbotk::Terminator], a [mlr3::Resampling] and a
#'    [mlr3::Measure].
#' 2. A final model is fit on the complete training data with the best found
#'    feature subset.
#'
#' During `$predict()` the `AutoFSelector` just calls the predict method of the
#' wrapped (inner) learner.
#'
#' Note that this approach allows to perform nested resampling by passing an
#' [AutoFSelector] object to [mlr3::resample()] or [mlr3::benchmark()].
#' To access the inner resampling results, set `store_fselect_instance = TRUE`
#' and execute [mlr3::resample()] or [mlr3::benchmark()] with
#' `store_models = TRUE` (see examples).
#'
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("holdout")
#' measure = msr("classif.ce")
#'
#' terminator = trm("evals", n_evals = 3)
#' fselector = fs("exhaustive_search")
#' afs = AutoFSelector$new(learner, resampling, measure, terminator, fselector,
#'   store_fselect_instance = TRUE)
#'
#' afs$train(task)
#' afs$model
#' afs$learner
#'
#' #  Nested resampling
#' afs = AutoFSelector$new(learner, resampling, measure, terminator, fselector,
#'   store_fselect_instance = TRUE)
#'
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(task, afs, resampling_outer, store_models = TRUE)
#'
#' # Aggregate performance of outer results
#' rr$aggregate()
#'
#' # Retrieve inner feature selection results
#' as.data.table(rr)$learner[[1]]$fselect_result
AutoFSelector = R6Class("AutoFSelector",
  inherit = Learner,
  public = list(

    #' @field instance_args (`list()`)\cr
    #' All arguments from construction to create the
    #' [FSelectInstanceSingleCrit].
    instance_args = NULL,

    #' @field fselector ([FSelector])\cr
    #' Stores the feature selection algorithm.
    fselector = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::Learner])\cr
    #' Learner to optimize the feature subset for, see
    #' [FSelectInstanceSingleCrit].
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #' Resampling strategy during feature selection, see
    #' [FSelectInstanceSingleCrit]. This [mlr3::Resampling] is meant to be the
    #' **inner** resampling, operating on the training set of an arbitrary outer
    #' resampling. For this reason it is not feasible to pass an instantiated
    #' [mlr3::Resampling] here.
    #'
    #' @param measure ([mlr3::Measure])\cr
    #' Performance measure to optimize.
    #'
    #' @param terminator ([bbotk::Terminator])\cr
    #' When to stop feature selection, see [FSelectInstanceSingleCrit].
    #'
    #' @param fselector ([FSelector])\cr
    #' Feature selection algorithm to run.
    #'
    #' @param store_fselect_instance (`logical(1)`)\cr
    #' If `TRUE` (default), stores the internally created
    #' [FSelectInstanceSingleCrit] with all intermediate results in slot
    #' `$fselect_instance`.
    initialize = function(learner, resampling, measure, terminator, fselector,
      store_fselect_instance = TRUE, store_benchmark_result = TRUE,
      store_models = FALSE, check_values = FALSE) {
      ia = list()
      ia$learner = assert_learner(learner)$clone(deep = TRUE)
      ia$resampling = assert_resampling(resampling,
        instantiated = FALSE)$clone()
      ia$measure = assert_measure(as_measure(measure), learner = learner)
      ia$terminator = assert_terminator(terminator)$clone()
      private$.store_fselect_instance = assert_flag(store_fselect_instance)
      ia$store_benchmark_result = assert_flag(store_benchmark_result)
      ia$store_models = assert_flag(store_models)

      if (!private$.store_fselect_instance && ia$store_benchmark_result) {
        stop("Benchmark results can only be stored if store_fselect_instance is set to TRUE")
      }
      if (ia$store_models && !ia$store_benchmark_result) {
        stop("Models can only be stored if store_benchmark_result is set to TRUE")
      }

      ia$check_values = assert_flag(check_values)
      self$instance_args = ia
      self$fselector = assert_r6(fselector, "FSelector")$clone()

      super$initialize(
        id = paste0(learner$id, ".fselector"),
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
      instance = invoke(FSelectInstanceSingleCrit$new, .args = ia)
      self$fselector$optimize(instance)

      feat = task$feature_names[as.logical(instance$result_x_search_space)]
      ia$task$select(feat)

      learner = ia$learner$clone(deep = TRUE)
      learner$train(ia$task)

      result_model = list(learner = learner)
      if (isTRUE(private$.store_fselect_instance)) {
        result_model$fselect_instance = instance
      }
      return(result_model)
    },

    .predict = function(task) {
      self$model$learner$predict(task)
    },

    .store_fselect_instance = NULL
  ),

  active = list(

    #' @field archive ([ArchiveFSelect)\cr
    #' Returns [FSelectInstanceSingleCrit] archive.
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

    #' @field fselect_instance ([FSelectInstanceSingleCrit])\cr
    #' Internally created feature selection instance with all intermediate
    #' results.
    fselect_instance = function() self$model$fselect_instance,

    #' @field fselect_result (named `list()`)\cr
    #' Short-cut to `$result` from [FSelectInstanceSingleCrit].
    fselect_result = function() self$fselect_instance$result
  )
)
