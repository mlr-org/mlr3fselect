#' @examples
#' library(mlr3)
#'
#' terminator = term("evals", n_evals = 10)
#' instance = FSelectInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' fs = opt("<%= id %>")
#' fs$optimize(instance)
#' instance$result
#' instance$archive$data



