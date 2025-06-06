#' @title Syntactic Sugar for Feature Selection Objects Construction
#'
#' @description
#' Functions to retrieve objects, set parameters and assign to fields in one go.
#' Relies on [mlr3misc::dictionary_sugar_get()] to extract objects from the respective [mlr3misc::Dictionary]:
#'
#' * `fs()` for a [FSelector] from [mlr_fselectors].
#' * `fss()` for a list of [FSelectors][FSelector] from [mlr_fselectors].
#' * `trm()` for a [bbotk::Terminator] from [mlr_terminators].
#' * `trms()` for a list of [Terminators][bbotk::Terminator] from [mlr_terminators].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [R6::R6Class] object of the respective type, or a list of [R6::R6Class] objects for the plural versions.
#'
#' @export
#' @examples
#' # random search fselector with batch size of 5
#' fs("random_search", batch_size = 5)
#'
#' # run time terminator with 20 seconds
#' trm("run_time", secs = 20)
fs = function(.key, ...) {
  dictionary_sugar_get(mlr_fselectors, .key, ...)
}

#' @rdname fs
#' @export
fss = function(.keys, ...) {
  dictionary_sugar_mget(mlr_fselectors, .keys, ...)
}

#' @title Syntactic Sugar for Feature Selection Instance Construction
#'
#' @description
#' Function to construct a [FSelectInstanceBatchSingleCrit] or [FSelectInstanceBatchMultiCrit].
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [FSelectInstanceBatchSingleCrit] and multiple measures a [FSelectInstanceBatchMultiCrit].
#'   If `NULL`, default measure is used.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_terminator
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_ties_method
#'
#' @inheritSection FSelectInstanceBatchSingleCrit Resources
#' @inheritSection FSelectInstanceBatchSingleCrit Default Measures
#'
#' @export
#' @inherit FSelectInstanceBatchSingleCrit examples
fsi = function(
  task,
  learner,
  resampling,
  measures = NULL,
  terminator,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features"
  ) {
  if (is.null(measures) || inherits(measures, "Measure")) {
    FSelectInstanceBatchSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks,
      ties_method = ties_method
    )
  } else {
    FSelectInstanceBatchMultiCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks)
  }
}

#' @title Syntactic Sugar for Asynchronous Feature Selection Instance Construction
#'
#' @description
#' Function to construct a [FSelectInstanceAsyncSingleCrit] or [FSelectInstanceAsyncMultiCrit].
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [FSelectInstanceAsyncSingleCrit] and multiple measures a [FSelectInstanceAsyncMultiCrit].
#'   If `NULL`, default measure is used.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_terminator
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_rush
#' @template param_ties_method
#'
#' @inheritSection FSelectInstanceBatchSingleCrit Resources
#' @inheritSection FSelectInstanceBatchSingleCrit Default Measures
#'
#' @export
#' @inherit FSelectInstanceBatchSingleCrit examples
fsi_async = function(
  task,
  learner,
  resampling,
  measures = NULL,
  terminator,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features",
  rush = NULL
  ) {
  if (is.null(measures) || inherits(measures, "Measure")) {
    FSelectInstanceAsyncSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks,
      rush = rush,
      ties_method = ties_method)
  } else {
    FSelectInstanceAsyncMultiCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks,
      rush = rush)
  }
}
