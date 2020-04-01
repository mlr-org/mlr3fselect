#' @title FSelectInstance Class
#'
#' @description
#' Specifies a general feature selection scenario.
#'
#' Evaluations of feature sets are performed in batches. Before and after a
#' batch is evaluated, the [Terminator] is queried for the remaining budget. If
#' the available budget is exhausted, an exception is raised, and no further
#' evaluations can be performed from this point on.
#'
#' A list of measures can be passed to the instance, and they will always be all
#' evaluated. However, single-criteria tuners optimize only the first measure.
#'
#' The [FSelect] object is also supposed to store its final result, consisting
#' of a selected feature set and associated estimated performance values, by
#' calling the method `instance$assign_result()`.
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
FSelectInstance = R6Class("FSelectInstance",
  public = list(
    #' @field task ([mlr3::Task]).
    task = NULL,

    #' @field learner ([mlr3::Learner]).
    learner = NULL,

    #' @field resampling ([mlr3::Resampling])
    resampling = NULL,

    #' @field measures (list of [mlr3::Measure]).
    measures = NULL,

    #' @field terminator ([Terminator]).
    terminator = NULL,

    #' @field evaluator ([bbotk::Evaluator]).
    evaluator = NULL,

    #' @field store_models (`logical(1)`).
    store_models = FALSE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param task [mlr3::Task]
    #' @param learner [mlr3::Learner]
    #' @param resampling [mlr3::Resampling]
    #' Note that uninstantiated resamplings are instantiated during construction
    #' so that all configurations.
    #' @param measures list of [mlr3::Measure]
    #' @param terminator [Terminator]
    initialize = function(task, learner, resampling, measures, terminator) {

      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE),
        task = self$task)
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE),
        task = self$task, learner = self$learner)
      terminator = assert_terminator(terminator)
      if (!resampling$is_instantiated) {
        self$resampling$instantiate(self$task)
      }

      fun = function(xdt) {
        tasks = map(seq_row(xdt), function(x) {
          state = self$task$feature_names[as.logical(xdt[x, ])]
          tsk = self$task$clone(deep = TRUE)
          tsk$select(state)
          return(tsk)
        })

        design = benchmark_grid(tasks = tasks, self$learner, self$resampling)
        bmr = benchmark(design, store_models = self$store_models)
        bmr_data = split(bmr$data, by = "uhash")
        aggr = bmr$aggregate(self$measures)

        cbind(xdt,
          aggr[, self$measures[[1]]$id, with = FALSE],
          bmr_data = bmr_data)
      }

      minimize = sapply(self$measures, function(s) s$minimize)
      names(minimize) = unlist(sapply(self$measures, function(s) s$id))

      domain = ParamSet$new(lapply(task$feature_names,
        function(s) ParamLgl$new(id = s)))

      objective = Objective$new(id = "feature_selection",
        fun = fun,
        domain = domain,
        minimize = minimize)
      archive = Archive$new(objective)
      self$evaluator = Evaluator$new(objective, archive, terminator)
    },

    #' @description
    #' Helper for print outputs.
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
    },

    #' @description
    #' The [FSelect] object writes the best found feature set
    #' and estimated performance values here. For internal use.
    #' @param feat `character`
    #' Must be character vector of feature names existing in `task`
    #' @param perf `numeric`
    #' Must be named numeric of performance measures, named with performance
    #' IDs, regarding all elements in `measures`.
    assign_result = function(feat, perf) {
      assert_names(feat, subset.of = self$task$feature_names)
      assert_numeric(perf)
      assert_names(names(perf), permutation.of = ids(self$measures))
      private$.result = list(feat = feat, perf = perf)
    }
  ),
  active = list(
    #' @field result Result of the feature selection i.e. the optimal feature
    #'   set and its estimated performances.
    result = function() {
      list(feat = private$.result$feat, perf = private$.result$perf)
    }
  ),

  private = list(
    .result = NULL
  )
)
