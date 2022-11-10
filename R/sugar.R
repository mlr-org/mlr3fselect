#' @title Syntactic Sugar for FSelect Construction
#'
#' @description
#' This function complements [mlr_fselectors] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [FSelector].
#' @export
#' @examples
#' fs("sequential", max_features = 4)
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
#'
#' @inheritSection FSelectInstanceSingleCrit Resources
#'
#' @export
#' @inherit FSelectInstanceSingleCrit examples
fsi = function(task, learner, resampling, measures = NULL, terminator, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE) {
  FSelectInstance = if (!is.list(measures)) FSelectInstanceSingleCrit else FSelectInstanceMultiCrit
  FSelectInstance$new(task, learner, resampling, measures, terminator, store_benchmark_result, store_models, check_values)
}
