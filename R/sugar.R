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
  dictionary_sugar(mlr_fselectors, .key, ...)
}

#' @rdname fs
#' @export
fss = function(.keys, ...) {
  dictionary_sugar_mget(mlr_fselectors, .keys, ...)
}

#' @title Syntactic Sugar for Automatic Feature Selection
#'
#' @description
#' Function to create an [AutoFSelector] object.
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve fselector from [mlr_fselectors] dictionary.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the fselector.
#'
#' @return [AutoFSelector]
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#'
#' @export
#' @examples
#' at = auto_fselector(
#'   method = "random_search",
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 50,
#'   batch_size = 10)
#'
#' at$train(tsk("pima"))
auto_fselector = function(method, learner, resampling, measure, term_evals = NULL, term_time = NULL, ...) {
  assert_choice(method, mlr_fselectors$keys())
  fselector = fs(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  AutoFSelector$new(learner, resampling, measure, terminator, fselector)
}

#' @title Function for Feature Selection
#'
#' @description
#' Function to optimize the feature set of a [mlr3::Learner].
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve fselector from [mlr_fselectors] dictionary.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the fselector.
#'
#' @return `FSelectInstanceSingleCrit`
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#'
#' @export
#' @examples
#' task = tsk("pima")
#'
#' instance = fselect(
#'   method = "random_search",
#'   task = task,
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 50,
#'   batch_size = 10)
#'
#' # subset task to optimized feature set
#' task$select(instance$result_feature_set)
fselect = function(method, task, learner, resampling, measure, term_evals = NULL, term_time = NULL,
  ...) {
  assert_choice(method, mlr_fselectors$keys())
  fselector = fs(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  instance = FSelectInstanceSingleCrit$new(task, learner, resampling, measure, terminator)

  fselector$optimize(instance)
  instance
}

#' @title Function for Nested Resampling
#'
#' @description
#' Function to conduct nested resampling.
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve fselector from [mlr_fselectors] dictionary.
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  Resampling used for the inner loop.
#' @param outer_resampling [mlr3::Resampling])\cr
#'  Resampling used for the outer loop.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the fselector.
#'
#' @return [mlr3::ResampleResult]
#'
#' @template param_task
#' @template param_learner
#' @template param_measure
#'
#' @export
#' @examples
#' rr = fselect_nested(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart"),
#'   inner_resampling = rsmp ("holdout"),
#'   outer_resampling = rsmp("cv", folds = 2),
#'   measure = msr("classif.ce"),
#'   term_evals = 2,
#'   batch_size = 2)
#'
#' # performance scores estimated on the outer resampling
#' rr$score()
#'
#' # unbiased performance of the final model trained on the full data set
#' rr$aggregate()
fselect_nested = function(method, task, learner, inner_resampling, outer_resampling, measure, term_evals = NULL,
  term_time = NULL, ...) {
  assert_task(task)
  assert_resampling(inner_resampling)
  assert_resampling(outer_resampling)

  afs = auto_fselector(method, learner, inner_resampling, measure, search_space, term_evals, term_time, ...)
  resample(task, afs, outer_resampling, store_models = TRUE)
}

terminator_selection = function(term_evals, term_time) {
  assert_int(term_evals, null.ok = TRUE)
  assert_int(term_time, null.ok = TRUE)

  if (is.null(term_evals) && is.null(term_time)) {
    trm("none")
  } else if (!is.null(term_evals) && !is.null(term_time)) {
    trm("combo", list(trm("evals", n_evals = term_evals), trm("run_time", secs = term_time)))
  } else if (!is.null(term_evals)) {
    trm("evals", n_evals = term_evals)
  } else if (!is.null(term_time)) {
    trm("run_time", secs = term_time)
  }
}
