#' @title Class for Single Criterion Feature Selection
#'
#' @include ArchiveBatchFSelect.R
#'
#' @description
#' The [FSelectInstanceBatchSingleCrit] specifies a feature selection problem for a [FSelector].
#' The function [fsi()] creates a [FSelectInstanceBatchSingleCrit] and the function [fselect()] creates an instance internally.
#'
#' @description
#' The instance contains an [ObjectiveFSelectBatch] object that encodes the black box objective function a [FSelector] has to optimize.
#' The instance allows the basic operations of querying the objective at design points (`$eval_batch()`).
#' This operation is usually done by the [FSelector].
#' Evaluations of feature subsets are performed in batches by calling [mlr3::benchmark()] internally.
#' The evaluated feature subsets are stored in the [Archive][ArchiveBatchFSelect] (`$archive`).
#' Before a batch is evaluated, the [bbotk::Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#' The [FSelector] is also supposed to store its final result, consisting of a selected feature subset and associated estimated performance values, by calling the method `instance$assign_result()`.
#'
#' @section Default Measures:
#' If no measure is passed, the default measure is used.
#' The default measure depends on the task type.
#'
#' | Task           | Default Measure     | Package               |
#' |----------------|---------------------|-----------------------|
#' | `"classif"`    | `"classif.ce"`      | \CRANpkg{mlr3}        |
#' | `"regr"`       | `"regr.mse"`        | \CRANpkg{mlr3}        |
#' | `"surv"`       | `"surv.cindex"`     | \CRANpkg{mlr3proba}   |
#' | `"dens"`       | `"dens.logloss"`    | \CRANpkg{mlr3proba}   |
#' | `"classif_st"` | `"classif.ce"`      | \CRANpkg{mlr3spatial} |
#' | `"regr_st"`    | `"regr.mse"`        | \CRANpkg{mlr3spatial} |
#' | `"clust"`      | `"clust.dunn"`      | \CRANpkg{mlr3cluster} |
#'
#' @inheritSection ArchiveBatchFSelect Analysis
#'
#' @section Resources:
#' There are several sections about feature selection in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' * Getting started with [wrapper feature selection](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-fs-wrapper).
#' * Do a [sequential forward selection](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-fs-wrapper-example) Palmer Penguins data set.
#'
#' The [gallery](https://mlr-org.com/gallery.html) features a collection of case studies and demos about optimization.
#'
#' * Utilize the built-in feature importance of models with [Recursive Feature Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).
#' * Run a feature selection with [Shadow Variable Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_callbacks
#' @template param_xdt
#' @template param_ties_method
#'
#' @export
#' @examples
#' # Feature selection on Palmer Penguins data set
#' \donttest{
#'
#' task = tsk("penguins")
#' learner = lrn("classif.rpart")
#'
#' # Construct feature selection instance
#' instance = fsi(
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # Choose optimization algorithm
#' fselector = fs("random_search", batch_size = 2)
#'
#' # Run feature selection
#' fselector$optimize(instance)
#'
#' # Subset task to optimal feature set
#' task$select(instance$result_feature_set)
#'
#' # Train the learner with optimal feature set on the full data set
#' learner$train(task)
#'
#' # Inspect all evaluated sets
#' as.data.table(instance$archive)
#' }
FSelectInstanceBatchSingleCrit = R6Class("FSelectInstanceBatchSingleCrit",
  inherit = OptimInstanceBatchSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measure,
      terminator,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      ties_method = "least_features"
      ) {
      # initialized specialized fselect archive and objective
      archive = ArchiveBatchFSelect$new(
        search_space = task_to_domain(assert_task(task)),
        codomain = measures_to_codomain(assert_measure(measure)),
        check_values = check_values,
        ties_method = ties_method)

      objective = ObjectiveFSelectBatch$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measure,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values = check_values,
        archive = archive,
        callbacks = callbacks)

      super$initialize(objective, objective$domain, terminator, callbacks = callbacks)

      # super class of instance initializes default archive, overwrite with fselect archive
      self$archive = archive

      private$.objective_function = objective_function
    },

    #' @description
    #' The [FSelector] writes the best found feature subset and estimated performance value here.
    #' For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #'   Optimal outcome.
    #' @param extra (`data.table::data.table()`)\cr
    #' Additional information.
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, y, extra = NULL, ...) {
      # Add feature names to result for easy task subsetting
      feature_names = self$objective$task$feature_names
      features = list(feature_names[as.logical(xdt[, feature_names, with = FALSE])])
      set(xdt, j = "features", value = list(features))
      set(xdt, j = "n_features", value = length(features[[1L]]))
      super$assign_result(xdt, y, extra = extra)
      if (!is.null(private$.result$x_domain)) set(private$.result, j = "x_domain", value = NULL)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function(...) {
      is_optimized = if (is.null(private$.result)) "Not optimized" else "Optimized"
      cat_cli({
        cli_h1("{.cls {class(self)[1L]}}")
        cli_li("State: {is_optimized}")
        cli_li("Objective: {.cls {class(self$objective)[1]}} ({self$objective$id})")
        cli_li("Terminator: {.cls {class(self$terminator)[1]}}")
      })

      if (!is.null(private$.result)) {
        cat_cli(cli_li("Result:"))
        print(self$result[, c(self$archive$cols_x, self$archive$cols_y), with = FALSE])
        cat_cli(cli_li("Archive:"))
        print(as.data.table(self$archive)[, c(self$archive$cols_x, self$archive$cols_y), with = FALSE])
      }
    }
  ),

  active = list(
    #' @field result_feature_set (`character()`)\cr
    #' Feature set for task subsetting.
    result_feature_set = function() {
      unlist(self$result$features)
    }
  ),

  private = list(
    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextBatchFSelect$new(self, optimizer)
      self$objective$context = context
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
