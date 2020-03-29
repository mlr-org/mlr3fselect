#' @examples
#' library(mlr3)
#'
#' terminator = term("evals", n_evals = 10)
#' instance = FSelectInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measures = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' fs = fs(<%= fs %>)
#' fs$select(instance)
#' instance$result
#' instance$archive()
#' instance$optimization_path()



