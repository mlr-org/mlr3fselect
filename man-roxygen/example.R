#' @examples
#' # retrieve task
#' task = tsk("pima")
#'
#' # load learner
#' learner = lrn("classif.rpart")
#'
#' \donttest{
#' # feature selection on the pima indians diabetes data set
#' instance = fselect(
#'   method = "<%= id %>",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 10
#' )
#'
#' # best performing feature subset
#' instance$result
#'
#' # all evaluated feature subsets
#' as.data.table(instance$archive)
#'
#' # subset the task and fit the final model
#' task$select(instance$result_feature_set)
#' learner$train(task)
#' }
