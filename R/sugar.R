#' @title Syntactic Sugar for Terminator Construction
#'
#' @description
#' This function complements [mlr_terminators] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [Terminator].
#' @export
#' @examples
#' term("evals", n_evals = 10)
term = function(.key, ...) {
  dictionary_sugar(mlr_terminators, .key, ...)
}

#' @title Syntactic Sugar for FSelect Construction
#'
#' @description
#' This function complements [mlr_fselectors] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [FSelect].
#' @export
#' @examples
#' fs("sequential", max_features = 4)
fs = function(.key, ...) {
  dictionary_sugar(mlr_fselectors, .key, ...)
}
