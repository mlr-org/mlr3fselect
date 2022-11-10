#' @title Multi Criterion Feature Selection Instance
#'
#' @include FSelectInstanceSingleCrit.R ArchiveFSelect.R
#'
#' @description
#' The [FSelectInstanceMultiCrit] specifies a feature selection problem for [FSelectors][FSelector].
#' The function [fsi()] creates a [FSelectInstanceMultiCrit] and the function [fselect()] creates an instance internally.
#'
#' @inherit FSelectInstanceSingleCrit details
#' @inheritSection FSelectInstanceSingleCrit Resources
#' @inheritSection ArchiveFSelect Analysis
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_xdt
#'
#'
#' @export
#' @examples
#' # Feature selection on Palmer Penguins data set
#' task = tsk("penguins")
#'
#' # Construct feature selection instance
#' instance = fsi(
#'   task = task,
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msrs(c("classif.ce", "time_train")),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # Choose optimization algorithm
#' fselector = fs("random_search", batch_size = 2)
#'
#' # Run feature selection
#' fselector$optimize(instance)
#'
#' # Optimal feature sets
#' instance$result_feature_set
#'
#' # Inspect all evaluated sets
#' as.data.table(instance$archive)
FSelectInstanceMultiCrit = R6Class("FSelectInstanceMultiCrit",
  inherit = OptimInstanceMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures, terminator, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE) {
      # initialized specialized fselect archive and objective
      archive = ArchiveFSelect$new(
        search_space = task_to_domain(assert_task(task)),
        codomain = measures_to_codomain(assert_measures(measures)),
        check_values = check_values)

      objective = ObjectiveFSelect$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        archive = archive)

      super$initialize(objective, objective$domain, terminator)

      # super class of instance initializes default archive, overwrite with fselect archive
      self$archive = archive

      private$.objective_function = objective_function
    },

    #' @description
    #' The [FSelector] object writes the best found feature subsets
    #' and estimated performance values here. For internal use.
    #'
    #' @param ydt (`data.table::data.table()`)\cr
    #' Optimal outcomes, e.g. the Pareto front.
    assign_result = function(xdt, ydt) {
      # Add feature names to result for easy task subsetting
      features = map(transpose_list(xdt), function(x) {
        self$objective$task$feature_names[as.logical(x)]
      })
      xdt[, features := list(features)]
      assert_data_table(xdt)
      assert_names(names(xdt), must.include = self$search_space$ids())
      assert_data_table(ydt)
      assert_names(names(ydt), permutation.of = self$objective$codomain$ids())
      private$.result = cbind(xdt, ydt)
    }
  ),

  active = list(
    #' @field result_feature_set (`list()` of `character()`)\cr
    #' Feature sets for task subsetting.
    result_feature_set = function() {
      map(self$result$features, function(x) {
        unlist(x)
      })
    }
  )
)

objective_function = function(x, inst, multiplicator) {
  xs = set_names(as.list(as.logical(x)), inst$search_space$ids())
  inst$search_space$assert(xs)
  xdt = as.data.table(xs)
  res = inst$eval_batch(xdt)
  y = as.numeric(res[, inst$archive$cols_y, with = FALSE])
  y * multiplicator
}
