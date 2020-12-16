#' @examples
#' library(mlr3)
#'
#' terminator = trm("evals", n_evals = 5)
#'
#' instance = FSelectInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' fselector = fs("<%= id %>")
#' \donttest{
#' # Modifies the instance by reference
#' fselector$optimize(instance)
#'
#' # Returns best scoring evaluation
#' instance$result
#'
#' # Allows access of data.table of full path of all evaluations
#' as.data.table(instance$archive)
