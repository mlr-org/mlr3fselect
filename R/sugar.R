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
