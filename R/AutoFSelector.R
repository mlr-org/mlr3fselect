#' @title Class for Automatic Feature Selection
#'
#' @description
#' The [AutoFSelector] wraps a [mlr3::Learner] and augments it with an automatic feature selection.
#' The [auto_fselector()] function creates an [AutoFSelector] object.
#'
#' @details
#' The [AutoFSelector] is a [mlr3::Learner] which wraps another [mlr3::Learner] and performs the following steps during `$train()`:
#'
#' 1. The wrapped (inner) learner is trained on the feature subsets via resampling.
#'    The feature selection can be specified by providing a [FSelector], a [bbotk::Terminator], a [mlr3::Resampling] and a [mlr3::Measure].
#' 2. A final model is fit on the complete training data with the best-found feature subset.
#'
#' During `$predict()` the [AutoFSelector] just calls the predict method of the wrapped (inner) learner.
#'
#' @section Resources:
#' There are several sections about feature selection in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' * Estimate Model Performance with [nested resampling](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-autofselect).
#'
#' The [gallery](https://mlr-org.com/gallery.html) features a collection of case studies and demos about optimization.
#'
#' @section Nested Resampling:
#' Nested resampling can be performed by passing an [AutoFSelector] object to [mlr3::resample()] or [mlr3::benchmark()].
#' To access the inner resampling results, set `store_fselect_instance = TRUE` and execute [mlr3::resample()] or [mlr3::benchmark()] with `store_models = TRUE` (see examples).
#' The [mlr3::Resampling] passed to the [AutoFSelector] is meant to be the inner resampling, operating on the training set of an arbitrary outer resampling.
#' For this reason it is not feasible to pass an instantiated [mlr3::Resampling] here.
#'
#' @template param_fselector
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_store_fselect_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_ties_method
#' @template param_rush
#' @template param_id
#'
#' @export
#' @examples
#' # Automatic Feature Selection
#' \donttest{
#'
#' # split to train and external set
#' task = tsk("penguins")
#' split = partition(task, ratio = 0.8)
#'
#' # create auto fselector
#' afs = auto_fselector(
#'   fselector = fs("random_search"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # optimize feature subset and fit final model
#' afs$train(task, row_ids = split$train)
#'
#' # predict with final model
#' afs$predict(task, row_ids = split$test)
#'
#' # show result
#' afs$fselect_result
#'
#' # model slot contains trained learner and fselect instance
#' afs$model
#'
#' # shortcut trained learner
#' afs$learner
#'
#' # shortcut fselect instance
#' afs$fselect_instance
#'
#'
#' # Nested Resampling
#'
#' afs = auto_fselector(
#'   fselector = fs("random_search"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' resampling_outer = rsmp("cv", folds = 3)
#' rr = resample(task, afs, resampling_outer, store_models = TRUE)
#'
#' # retrieve inner feature selection results.
#' extract_inner_fselect_results(rr)
#'
#' # performance scores estimated on the outer resampling
#' rr$score()
#'
#' # unbiased performance of the final model trained on the full data set
#' rr$aggregate()
#' }
AutoFSelector = R6Class("AutoFSelector",
  inherit = Learner,
  public = list(

    #' @field instance_args (`list()`)\cr
    #' All arguments from construction to create the [FSelectInstanceBatchSingleCrit].
    instance_args = NULL,

    #' @field fselector ([FSelector])\cr
    #' Optimization algorithm.
    fselector = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param fselector ([FSelector])\cr
    #'   Optimization algorithm.
    initialize = function(
      fselector,
      learner,
      resampling,
      measure = NULL,
      terminator,
      store_fselect_instance = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      ties_method = "least_features",
      rush = NULL,
      id = NULL
      ) {
      ia = list()
      self$fselector = assert_r6(fselector, "FSelector")$clone()
      ia$learner = assert_learner(as_learner(learner, clone = TRUE))
      ia$resampling = assert_resampling(resampling, instantiated = FALSE)$clone()
      if (!is.null(measure)) ia$measure = assert_measure(as_measure(measure), learner = learner)
      ia$terminator = assert_terminator(terminator)$clone()

      ia$store_models = assert_flag(store_models)
      ia$store_benchmark_result = assert_flag(store_benchmark_result) || ia$store_models
      private$.store_fselect_instance = assert_flag(store_fselect_instance) || ia$store_benchmark_result

      ia$check_values = assert_flag(check_values)
      ia$callbacks = assert_callbacks(as_callbacks(callbacks))
      if (!is.null(rush)) ia$rush = assert_class(rush, "Rush")
      ia$ties_method = assert_choice(ties_method, c("least_features", "random"))
      self$instance_args = ia

      id = assert_string(id, null.ok = TRUE) %??% paste0(learner$id, ".fselector")

      super$initialize(
        id = id,
        task_type = learner$task_type,
        packages = c("mlr3fselect", learner$packages),
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        properties = setdiff(learner$properties, c("validation", "internal_tuning"))
      )

      self$predict_type = learner$predict_type
      self$predict_sets = learner$predict_sets
    },

    #' @description
    #' Extracts the base learner from nested learner objects like `GraphLearner` in \CRANpkg{mlr3pipelines}.
    #' If `recursive = 0`, the (tuned) learner is returned.
    #'
    #' @param recursive (`integer(1)`)\cr
    #'   Depth of recursion for multiple nested objects.
    #'
    #' @return [mlr3::Learner].
    base_learner = function(recursive = Inf) {
      if (recursive == 0L) self$learner else self$learner$base_learner(recursive - 1L)
    },

    #' @description
    #' The importance scores of the final model.
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if ("importance" %nin% self$instance_args$learner$properties) {
        stopf("Learner ''%s' cannot calculate important scores.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$importance()
      } else {
        self$model$learner$importance()
      }
    },

    #' @description
    #' The selected features of the final model.
    #' These features are selected internally by the learner.
    #'
    #' @return `character()`.
    selected_features = function() {
      if ("selected_features" %nin% self$instance_args$learner$properties) {
        stopf("Learner ''%s' cannot select features.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$selected_features()
      } else {
        self$model$learner$selected_features()
      }
    },

    #' @description
    #' The out-of-bag error of the final model.
    #'
    #' @return `numeric(1)`.
    oob_error = function() {
      if ("oob_error" %nin% self$instance_args$learner$properties) {
        stopf("Learner '%s' cannot calculate the out-of-bag error.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$oob_error()
      } else {
        self$model$learner$oob_error()
      }
    },

    #' @description
    #' The log-likelihood of the final model.
    #'
    #' @return `logLik`.
    loglik = function() {
      if ("loglik" %nin% self$instance_args$learner$properties) {
        stopf("Learner '%s' cannot calculate the log-likelihood.", self$instance_args$learner$id)
      }
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner$loglik()
      } else {
        self$model$learner$loglik()
      }
    },

    #' Printer.
    #' @param ... (ignored).
    print = function() {
      msg_h =  if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label)
      model = if (is.null(self$model)) "-" else class(self$model)[1L]

      cat_cli({
        cli_h1("{.cls {class(self)[1L]}} ({self$id}){msg_h}")
        cli_li("Model: {model}")
        cli_li("Packages: {.pkg {self$packages}}")
        cli_li("Predict Type: {self$predict_type}")
        cli_li("Feature Types: {self$feature_types}")
        cli_li("Properties: {self$properties}")
      })

      w = self$warnings
      e = self$errors
      if (length(w)) {
        cat_cli(cli_alert_warning("Warnings: {w}"))
      }
      if (length(e)) {
        cat_cli(cli_alert_danger("Errors: {e}"))
      }
    }
  ),

  active = list(

    #' @field archive ([ArchiveBatchFSelect)\cr
    #' Returns [FSelectInstanceBatchSingleCrit] archive.
    archive = function() self$fselect_instance$archive,

    #' @field learner ([mlr3::Learner])\cr
    #' Trained learner.
    learner = function() {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model$learner$model)) {
        self$instance_args$learner
      } else {
        self$model$learner
      }
    },

    #' @field fselect_instance ([FSelectInstanceBatchSingleCrit])\cr
    #' Internally created feature selection instance with all intermediate results.
    fselect_instance = function() self$model$fselect_instance,

    #' @field fselect_result ([data.table::data.table])\cr
    #' Short-cut to `$result` from [FSelectInstanceBatchSingleCrit].
    fselect_result = function() self$fselect_instance$result,

    #' @field predict_type (`character(1)`)\cr
    #' Stores the currently active predict type, e.g. `"response"`.
    #' Must be an element of `$predict_types`.
    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }
      if (rhs %nin% self$predict_types) {
        stopf("Learner '%s' does not support predict type '%s'", self$id, rhs)
      }

      # Catches 'Error: Field/Binding is read-only' bug
      tryCatch({
        self$model$learner$predict_type = rhs
      }, error = function(cond){})

      private$.predict_type = rhs
    },

    #' @field hash (`character(1)`)\cr
    #' Hash (unique identifier) for this object.
    hash = function(rhs) {
      assert_ro_binding(rhs)
      calculate_hash(class(self), self$id, self$param_set$values, private$.predict_type, self$fallback$hash, self$parallel_predict, self$fselector, self$instance_args, private$.store_fselect_instance)
    },

    #' @field phash (`character(1)`)\cr
    #' Hash (unique identifier) for this partial object, excluding some components which are varied systematically during tuning (parameter values) or feature selection (feature names).
    phash = function(rhs) {
      assert_ro_binding(rhs)
      self$hash
    }
  ),

  private = list(
    .train = function(task) {
      # construct instance from args
      ia = self$instance_args
      ia$task = task$clone()

      # check if task contains all row ids required for instantiated resampling
      if (ia$resampling$is_instantiated) {
        imap(ia$resampling$instance$train, function(x, i) {
          if (!test_subset(x, task$row_ids)) {
            stopf("Train set %i of inner resampling '%s' contains row ids not present in task '%s': {%s}", i, ia$resampling$id, task$id, paste(setdiff(x, task$row_ids), collapse = ", "))
          }
        })

        imap(ia$resampling$instance$test, function(x, i) {
          if (!test_subset(x, task$row_ids)) {
            stopf("Test set %i of inner resampling '%s' contains row ids not present in task '%s': {%s}", i, ia$resampling$id, task$id, paste(setdiff(x, task$row_ids), collapse = ", "))
          }
        })
      }

      FSelectInstance = if (inherits(self$fselector, "FSelectorAsync")) FSelectInstanceAsyncSingleCrit else FSelectInstanceBatchSingleCrit
      instance = do.call(FSelectInstance$new, args = ia)

      # optimize feature selection
      self$fselector$optimize(instance)

      # make auto fselector available to callbacks
      instance$objective$context$auto_fselector = self
      call_back("on_auto_fselector_before_final_model", instance$objective$callbacks, instance$objective$context)

      learner = ia$learner$clone(deep = TRUE)
      task = task$clone()

      # disable timeout to allow train on full data set without time limit
      # timeout during optimization is not affected
      learner$timeout = c(train = Inf, predict = Inf)

      # fit final model
      feat = task$feature_names[as.logical(instance$result_x_search_space)]
      task$select(feat)
      learner$train(task)

      call_back("on_auto_fselector_after_final_model", instance$objective$callbacks, instance$objective$context)

      # the return model is a list of "learner", "features" and "fselect_instance"
      result_model = list(learner = learner, features = feat)
      if (private$.store_fselect_instance) result_model$fselect_instance = instance

      result_model
    },

    .predict = function(task) {
      task = task$clone(deep = TRUE)
      task$select(self$model$features)
      self$model$learner$predict(task)
    },

    .store_fselect_instance = NULL
  )
)
