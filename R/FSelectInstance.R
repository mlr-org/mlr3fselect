#' FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario.
#'
#' @export
FSelectInstance = R6Class("FSelectInstance",
  public = list(
    #' @field task [mlr3::Task]
    #' @field learner [mlr3::Learner]
    #' @field resampling [mlr3::Resampling]
    #' @field measures list of [mlr3::Measure]
    #' @field terminator [mlr3tuning::Terminator]
    #' @field bmr [mlr3::BenchmarkResult]
    task = NULL,
    learner = NULL,
    resampling = NULL,
    measures = NULL,
    terminator = NULL,
    bmr = NULL,

    #' @description
    #' Create new `FSelectInstance` object.
    #' @param task [mlr3::Task]
    #' @param learner [mlr3::Learner]
    #' @param resampling [mlr3::Resampling]
    #' @param measures list of [mlr3::Measure]
    #' @param terminator [Terminator]
    #' @param bmr [mlr3::BenchmarkResult]
    #' Stores all evaluated [mlr3::ResampleResult]s when evaluating feature combinations.
    #' @return A `FSelectInstance` object.
    initialize = function(task, learner, resampling, measures, terminator) {
      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE), task = self$task)
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$terminator = assert_terminator(terminator)
      self$bmr = BenchmarkResult$new(data.table())
      self$bmr$rr_data[, c("batch_nr") := list(integer())]
      self$resampling$instantiate(self$task)
    },

    #' @description
    #' Format method.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    print = function() {
      catf(self$format())
      catf(str_indent("* Task:", format(self$task)))
      catf(str_indent("* Learner:", format(self$learner)))
      catf(str_indent("* Measures:", map_chr(self$measures, "id")))
      catf(str_indent("* Resampling:", format(self$resampling)))
      catf(str_indent("* Terminator:", format(self$terminator)))
    },

    #' @description
    #' Evaluates all feature combinations in `states` through resampling.
    #' @param states (`matrix`) Each row represents a 0/1 encoded feature combination.
    eval_batch = function(states) {

      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }

      # Clone task and set feature subset
      tsks = apply(states, 1, function(x) {
        state = self$task$feature_names[as.logical(x)]
        tsk = self$task$clone(deep = TRUE)
        tsk$select(state)
        return(tsk)
      })

      # Evaluate states
      d = data.table::data.table(task = tsks, learner = list(self$learner), resampling = list(self$resampling))
      bmr = benchmark(d)

      # Add batch_nr
      batch_nr = self$bmr$rr_data$batch_nr
      batch_nr = if (length(batch_nr)) max(batch_nr) + 1L else 1L
      bmr$rr_data[, ("batch_nr") := batch_nr]

      # Store evaluated states
      self$bmr$combine(bmr)

      # Logger
      performance = bmr$aggregate(self$measures[[1]], ids = FALSE)[, self$measures[[1]]$id, with = FALSE]
      states = data.table(states)
      names(states) = self$task$feature_names

      lg$info("Result")
      lg$info(sprintf("Batch %d", batch_nr))
      lg$info(capture.output(print(cbind(states, performance), class = FALSE, row.names = FALSE, print.keys = FALSE)))

      if (self$terminator$is_terminated(self)) {
        stop(terminated_error(self))
      }
    },

    #' @description
    #' Queries the [mlr3::BenchmarkResult] for the `n` best feature combinations.
    #' @param n (`ìnteger`)
    #' @return A [data.table::data.table] object.
    best = function(n = 1) {

      assert_int(n, lower = 1)

      tab = self$bmr$aggregate(self$measures[[1]], ids = FALSE)
      order = if (self$measures[[1]]$minimize) 1 else -1
      setorderv(tab, self$measures[[1]]$id, order = order)

      res = t(sapply(tab[1:n]$resample_result, function(x) {
        as.numeric(self$task$feature_names %in% x$task$feature_names)
      }))

      res = as.data.table(res)
      names(res) = self$task$feature_names
      cbind(res, tab[1:n, self$measures[[1]]$id, with = FALSE])
    },

    #' @description
    #' Queries the [mlr3::BenchmarkResult] for the `n` best feature combinations in each batch.
    #' @param n (`ìnteger`)
    #' @return A [data.table::data.table] object.
    best_by_batch = function(n = 1) {

      assert_int(n, lower = 1)

      tab = self$bmr$aggregate(self$measures[[1]], ids = FALSE)
      order = if (self$measures[[1]]$minimize) 1 else -1
      setorderv(tab, c("batch_nr", self$measures[[1]]$id), order = order)
      tab = tab[, head(.SD, n), by = batch_nr]

      res = t(sapply(tab$resample_result, function(x) {
        as.numeric(self$task$feature_names %in% x$task$feature_names)
      }))

      res = as.data.table(res)
      names(res) = self$task$feature_names
      cbind(res, tab[, list(batch_nr), ], tab[, self$measures[[1]]$id, with = FALSE])
    }
  )
)
