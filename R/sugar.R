#' @title Syntactic Sugar for FSelect Construction
#'
#' @description
#' Functions to retrieve objects, set parameters and assign to fields in one go.
#' Relies on [mlr3misc::dictionary_sugar_get()] to extract objects from the respective [mlr3misc::Dictionary]:
#'
#' * `fs()` for a [FSelector] from [mlr_fselectors].
#' * `fss()` for a list of [FSelectors][FSelector] from [mlr_fselectors].
#' * `trm()` for a [Terminator] from [mlr_terminators].
#' * `trms()` for a list of [Terminators][Terminator] from [mlr_terminators].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [R6::R6Class] object of the respective type, or a list of [R6::R6Class] objects for the plural versions.
#'
#' @export
#' @examples
#' # random search with batch size of 5
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

#' @title Syntactic Sugar for Instance Construction
#'
#' @description
#' Function to construct a [FSelectInstanceSingleCrit] or [FSelectInstanceMultiCrit].
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [FSelectInstanceSingleCrit] and multiple measures a [FSelectInstanceMultiCrit].
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
#' @inheritSection FSelectInstanceSingleCrit Resources
#' @inheritSection FSelectInstanceSingleCrit Default Measures
#'
#' @export
#' @inherit FSelectInstanceSingleCrit examples
fsi = function(
  task,
  learner,
  resampling,
  measures = NULL,
  terminator,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = list(),
  ties_method = "least_features"
  ) {
  if (!is.list(measures)) {
    FSelectInstanceSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks,
      ties_method = ties_method)
  } else {
    FSelectInstanceMultiCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures = measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks)
  }
}
