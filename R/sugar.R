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
