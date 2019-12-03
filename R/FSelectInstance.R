#' FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario including performance evaluator
#' and archive for [FSelect] objects to act upon.
#' This class encodes the black box objective function that a [FSelect] object has to optimize.
#' It allows the basis operations of querying the objective with feature subsets (`$eval_batch()`),
#' storing the evaluations in an internal archive and querying the archive (`$archive()`).
#'
#' Evaluations of feature subsets are performed in batches by calling [mlr3::benchmark()] internally.
#' Before and after a batch is evaluated, the [Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised,
#' and no further evaluations can be performed from this point on.
#'
#' A list of measures can be passed to the instance, and they will always be all evaluated.
#' However, single-criteria tuners optimize only the first measure.
#'
#' The [FSelect] object is also supposed to store its final result,
#' consisting of a selected feature subset
#' and associated estimated performance values, by calling the method `instance$assign_result`.
#'
#' This class allows to display the optimization path of the feature selection
#' in varying levels of detail (`$optimization_path`).
#'
#' @export
FSelectInstance = R6Class("FSelectInstance",
  public = list(
    #' @field task [mlr3::Task]
    #' @field learner [mlr3::Learner]
    #' @field resampling [mlr3::Resampling]
    #' @field measures list of [mlr3::Measure]
    #' @field terminator [mlr3tuning::Terminator]
    #' @field bm_args named `list()`
    #' @field bmr [mlr3::BenchmarkResult]
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    terminator = NULL,
    bm_args = NULL,
    bmr = NULL,

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
    #' @return A `FSelectInstance` object.
    initialize = function(task, learner, resampling, measures, terminator, bm_args = list()) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE), task = self$task)
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$terminator = assert_terminator(terminator)
      self$bm_args = assert_list(bm_args, names = "unique")
      self$bmr = BenchmarkResult$new(data.table())
      self$bmr$rr_data[, c("batch_nr", "feat") := list(integer(), character())]
      self$resampling$instantiate(self$task)

      if (!resampling$is_instantiated) self$resampling$instantiate(self$task)
    },

    #' @description
    #' Format method.
    #' @return `character()`
    format = function() {
      sprintf("<%s>", class(self)[1L])
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
    },

    #' @description
    #' Evaluates all feature subsets in `states` through resampling.
    #' Updates the internal [BenchmarkResult] `$bmr` by reference.
    #' Before and after each batch-evaluation, the [Terminator] is checked,
    #' and if it is positive, an exception of class `terminated_error` is raised.
    #' This function should be internally called by the [FSelect] object.
    #' @param states (`matrix`)
    #' Each row represents a 0/1 encoded feature subset.
    #' @return named `list()`
    eval_batch = function(states) {

      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }

      assert_matrix(states, ncols = length(self$task$feature_names))

      lg$info("Evaluating %i feature subsets", nrow(states))

      # Clone task and set feature subset
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
      feat_list = lapply(tsks, function(x) {x$feature_names})
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
    #' Evaluates a feature subset `x` and returns a scalar objective value,
    #' where the return value is negated if the measure is maximized.
    #' Internally, `$eval_batch()` is called with a single row.
    #' This method is useful for feature selection algorithms that take a objective function.
    #' @param x `numeric`
    #' 0/1 encoded feature subset
    #' @return `numeric(1)`
    fselect_objective = function(x) {
      assert_numeric(x, len = length(self$task$feature_names))
      x = matrix(x, nrow=1)
      z = self$eval_batch(x)
      m = self$measures[[1L]]
      y = z$perf[[m$id]]
      if (m$minimize) y else -y
    },

    #' @description
    #' Returns a table of contained resample results with corresponding feature subsets.
    #' @return [data.table::data.table]
    archive = function() {
      self$bmr$aggregate(self$measures)
    },

    #' @description
    #' Queries the [ml3::BenchmarkResult] for the `n` best feature subsets (default is `1`)
    #' of the batches specified in `m` (default is the last batch).
    #' @param n (`integer`)
    #' @param m (`integer`)
    #' @return [data.table::data.table]
    optimization_path = function(n = 1, m = NULL) {
      assert_int(n, lower = 1)
      assert_integerish(m, null.ok = TRUE)
      if(is.null(m)) m = self$bmr$rr_data$batch_nr[length(self$bmr$rr_data$batch_nr)]

      tab = self$bmr$aggregate(self$measures[[1]], ids = FALSE)
      order = if (self$measures[[1]]$minimize) 1 else -1
      setorderv(tab, c("batch_nr", self$measures[[1]]$id), order = order)
      tab = tab[batch_nr %in% m, head(.SD, n), by = batch_nr]

      res = t(sapply(tab$resample_result, function(x) {
        as.numeric(self$task$feature_names %in% x$task$feature_names)
      }))

      res = as.data.table(res)
      names(res) = self$task$feature_names
      cbind(tab[,list(batch_nr)], res, tab[, self$measures[[1]]$id, with = FALSE])
    },

    #' @description
    #' Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult]
    #' according to `measure` (default is the first measure in `$measures`)
    #' of batch  `m` (default is the last batch).
    #' In case of ties, one of the tied values is selected randomly.
    #' @param measure [mlr3::Measure]
    #' @param m (`ìnteger`)
    #' @return [mlr3::ResampleResult]
    best = function(measure = NULL, m = NULL) {
      if (is.null(measure)) {
        measure = self$measures[[1L]]
      } else {
        measure = as_measure(measure, task_type = self$task$task_type)
        assert_choice(measure$id, map_chr(self$measures, "id"))
      }

      assert_int(m, null.ok = TRUE)
      if(is.null(m)) m = self$bmr$rr_data$batch_nr[length(self$bmr$rr_data$batch_nr)]

      tab = self$bmr$aggregate(measure, ids = FALSE)
      tab = tab[batch_nr == m]

      y = tab[[measure$id]]
      if (allMissing(y))
        stopf("No non-missing performance value stored")

      which_best = if (measure$minimize) which_min else which_max
      best_index = which_best(y, na_rm = TRUE)
      tab$resample_result[[best_index]]
    },

    #' @description
    #' The [FSelect] object writes the best found feature subset
    #' and estimated performance values here. For internal use.
    #' @param feat (`character`) Must be character vector of feature names existing in `task`
    #' @param perf (`numeric`) Must be named numeric of performance measures, named with performance IDs, regarding all elements in `measures`.
    #' @return `NULL`
    assign_result = function(feat, perf) {
      assert_names(feat, subset.of = self$task$feature_names)
      assert_numeric(perf)
      assert_names(names(perf), permutation.of = ids(self$measures))
      private$.result = list(feat = feat, perf = perf)
    }
  ),
  active = list(
    #' @field n_evals Number of evaluations.
    n_evals = function() self$bmr$n_resample_results,

    #' @field result Result of the feature selection i.e. the optimal feature  subset and its estimated performances.
    result = function() {
      list(feat = private$.result$feat, perf = private$.result$perf)
    }
  ),

  private = list(
    .result = NULL
  )
)
