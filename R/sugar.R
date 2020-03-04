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
