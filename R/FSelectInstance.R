#' FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario including performance evaluator
#' and archive for [FSelect] objects to act upon.
#' This class encodes the black box objective function that a [FSelect] object has to optimize.
#' It allows the basis operations of querying the objective with feature sets (`$eval_batch()`),
#' storing the evaluations in an internal archive and querying the archive (`$archive()`).
#'
#' Evaluations of feature sets are performed in batches by calling [mlr3::benchmark()] internally.
#' Before and after a batch is evaluated, the [Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised,
#' and no further evaluations can be performed from this point on.
#'
#' A list of measures can be passed to the instance, and they will always be all evaluated.
#' However, single-criteria tuners optimize only the first measure.
#'
#' The [FSelect] object is also supposed to store its final result,
#' consisting of a selected feature set
#' and associated estimated performance values, by calling the method `instance$assign_result()`.
#'
#' This class allows to display the optimization path of the feature selection
#' in varying levels of detail (`$optimization_path()`).
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' # Objects required to define the performance evaluator
#' task = tsk("iris")
#' measures = msrs(c("classif.ce"))
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#' terminator = term("evals", n_evals = 8)
#'
#' instance = FSelectInstance$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measures = measures,
#'   terminator = terminator
#' )
#'
#' # Evaluate 4 feature sets
#' m = diag(4)
#' instance$eval_batch(m)
#' instance$archive()
FSelectInstance = R6Class("FSelectInstance", inherit = Instance,
  public = list(

    #' @description
    #' Create new `FSelectInstance` object.
    #' @param task [mlr3::Task]
    #' @param learner [mlr3::Learner]
    #' @param resampling [mlr3::Resampling] Note that uninstantiated resamplings are instantiated during construction so that all configurations.
    #' @param measures list of [mlr3::Measure]
    #' @param terminator [Terminator]
    #' @param bmr [mlr3::BenchmarkResult]
    #' @param bm_args named `list()` Further arguments for [mlr3::benchmark()].
    #' Stores all evaluated [mlr3::ResampleResult]s when evaluating feature combinations.
    #' @return `FSelectInstance`
    initialize = function(task, learner, resampling, measures, terminator, bm_args = list()) {
      super$initialize(task, learner, resampling, measures, terminator, bm_args)
    },

    #' @description
    #' Print method.
    #' @return `character()`
    print = function() {
      catf(self$format())
      catf(str_indent("* Task:", format(self$task)))
      catf(str_indent("* Learner:", format(self$learner)))
      catf(str_indent("* Measures:", map_chr(self$measures, "id")))
      catf(str_indent("* Resampling:", format(self$resampling)))
      catf(str_indent("* Terminator:", format(self$terminator)))
      catf(str_indent("* bm_args:", as_short_string(self$bm_args)))
      catf("Archive:")
      print(self$archive())
    },

    #' @description
    #' Evaluates all feature sets in `states` through resampling.
    #' Updates the internal [BenchmarkResult] `$bmr` by reference.
    #' Before and after each batch-evaluation, the [Terminator] is checked,
    #' and if it is positive, an exception of class `terminated_error` is raised.
    #' This function should be internally called by the [FSelect] object.
    #' @param states `matrix`
    #' Each row represents a 0/1 encoded feature set.
    #' @return named `list()`
    eval_batch = function(states) {

      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }

      assert_matrix(states, ncols = length(self$task$feature_names))

      lg$info("Evaluating %i feature sets", nrow(states))

      # Clone task and set feature set
      tsks = apply(states, 1, function(x) {
        state = self$task$feature_names[as.logical(x)]
        tsk = self$task$clone(deep = TRUE)
        tsk$select(state)
        return(tsk)
      })

      # Evaluate states
      d = data.table::data.table(task = tsks, learner = list(self$learner), resampling = list(self$resampling))
      bmr = invoke(benchmark, d, .args = self$bm_args)

      # Add batch_nr to rr_data
      batch_nr = self$bmr$rr_data$batch_nr
      batch_nr = if (length(batch_nr)) max(batch_nr) + 1L else 1L
      bmr$rr_data[, ("batch_nr") := batch_nr]

      # Add feat to rr_data
      feat_list = lapply(tsks, function(x) {
        x$feature_names
      })
      bmr$rr_data[, ("feat") := list(feat_list)]

      # Store evaluated states
      self$bmr$combine(bmr)

      # Logger
      mids = ids(self$measures)
      perf = bmr$aggregate(self$measures, ids = FALSE)[, mids, with = FALSE]
      states = data.table(states)
      names(states) = self$task$feature_names

      lg$info("Result")
      lg$info(sprintf("Batch %d", batch_nr))
      lg$info(capture.output(print(cbind(states, perf), class = FALSE, row.names = FALSE, print.keys = FALSE)))

      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }
      return(list(batch_nr = batch_nr, uhashes = bmr$uhashes, perf = perf))
    },

    #' @description
    #' Evaluates a feature set `x` and returns a scalar objective value,
    #' where the return value is negated if the measure is maximized.
    #' Internally, `$eval_batch()` is called with a single row.
    #' This method is useful for feature selection algorithms that take a objective function.
    #' @param x `numeric`
    #' 0/1 encoded feature set
    #' @return `numeric(1)`
    fselect_objective = function(x) {
      assert_numeric(x, len = length(self$task$feature_names))
      x = matrix(x, nrow = 1)
      z = self$eval_batch(x)
      m = self$measures[[1L]]
      y = z$perf[[m$id]]
      if (m$minimize) y else -y
    },

    #' @description
    #' Returns a table of contained resample results with corresponding feature sets.
    #' @return [data.table::data.table]
    archive = function() {
      self$bmr$aggregate(self$measures)
    },

    #' @description
    #' Queries the [mlr3::BenchmarkResult] for the `n` best feature sets (default is `1`)
    #' of the batches specified in `m` (default is the last batch).
    #' @param n `integer(1)`
    #' Number of feature sets per batch.
    #' @param m `integer`
    #' Batch numbers.
    #' @return [data.table::data.table]
    optimization_path = function(n = 1, m = NULL) {

      if (self$n_batch == 0) {
        stop("No feature selection conducted")
      }

      assert_int(n, lower = 1)
      assert_integerish(m, null.ok = TRUE)
      if (is.null(m)) m = self$n_batch

      tab = self$bmr$aggregate(self$measures[[1]], ids = FALSE)
      order = if (self$measures[[1]]$minimize) 1 else -1
      setorderv(tab, c("batch_nr", self$measures[[1]]$id), order = order)
      tab = tab[batch_nr %in% m, head(.SD, n), by = batch_nr]

      res = data.table::transpose(map_dtc(tab$resample_result, function(x) {
        as.numeric(self$task$feature_names %in% x$task$feature_names)
      }))

      names(res) = self$task$feature_names
      cbind(tab[, list(batch_nr)], res, tab[, self$measures[[1]]$id, with = FALSE])
    },

    #' @description
    #' Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult]
    #' according to `measure` (default is the first measure in `$measures`
    #' of the batches specified in `m` (default is the all batches).
    #' In case of ties, one of the tied values is selected randomly.
    #' @param measure [mlr3::Measure]
    #' @param m `ìnteger`
    #' @return [mlr3::ResampleResult]
    best = function(measure = NULL, m = NULL) {

      if (self$n_batch == 0) {
        stop("No feature selection conducted")
      }

      if (is.null(measure)) {
        measure = self$measures[[1L]]
      } else {
        measure = as_measure(measure, task_type = self$task$task_type)
        assert_choice(measure$id, map_chr(self$measures, "id"))
      }
      assert_measure(measure, task = self$task, learner = self$learner)
      if (is.na(measure$minimize)) {
        stopf("Measure '%s' has minimize = NA and hence cannot be used for feature selection", measure$id)
      }

      assert_int(m, null.ok = TRUE)
      if (is.null(m)) m = 1:self$n_batch

      tab = self$bmr$aggregate(measure, ids = FALSE)
      tab = tab[batch_nr %in% m]

      y = tab[[measure$id]]
      if (allMissing(y)) {
        stopf("No non-missing performance value stored")
      }

      which_best = if (measure$minimize) which_min else which_max
      best_index = which_best(y, na_rm = TRUE)
      tab$resample_result[[best_index]]
    },

    #' @description
    #' The [FSelect] object writes the best found feature set
    #' and estimated performance values here. For internal use.
    #' @param feat `character` Must be character vector of feature names existing in `task`
    #' @param perf `numeric` Must be named numeric of performance measures, named with performance IDs, regarding all elements in `measures`.
    assign_result = function(feat, perf) {
      assert_names(feat, subset.of = self$task$feature_names)
      assert_numeric(perf)
      assert_names(names(perf), permutation.of = ids(self$measures))
      private$.result = list(feat = feat, perf = perf)
    }
  ),
  active = list(
    #' @field n_batch Number of batches.
    n_batch = function() {
      if (length(self$bmr$rr_data$batch_n) == 0) 0L else self$bmr$rr_data$batch_n[length(self$bmr$rr_data$batch_n)]
    },

    #' @field result Result of the feature selection i.e. the optimal feature set and its estimated performances.
    result = function() {
      list(feat = private$.result$feat, perf = private$.result$perf)
    }
  ),

  private = list(
    .result = NULL
  )
)
