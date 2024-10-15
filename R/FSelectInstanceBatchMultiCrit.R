#' @title Class for Multi Criteria Feature Selection
#'
#' @include FSelectInstanceBatchSingleCrit.R ArchiveBatchFSelect.R
#'
#' @description
#' The [FSelectInstanceBatchMultiCrit] specifies a feature selection problem for a [FSelector].
#' The function [fsi()] creates a [FSelectInstanceBatchMultiCrit] and the function [fselect()] creates an instance internally.
#'
#' @inherit FSelectInstanceBatchSingleCrit details
#' @inheritSection ArchiveBatchFSelect Analysis
#'
#' @section Resources:
#' There are several sections about feature selection in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'  * Learn about [multi-objective optimization](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#sec-multicrit-featsel).
#'
#' The [gallery](https://mlr-org.com/gallery.html) features a collection of case studies and demos about optimization.
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_callbacks
#' @template param_xdt
#'
#' @export
#' @examples
#' # Feature selection on Palmer Penguins data set
#' \donttest{
#'
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
#' }
FSelectInstanceBatchMultiCrit = R6Class("FSelectInstanceBatchMultiCrit",
  inherit = OptimInstanceBatchMultiCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures, terminator, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, callbacks = NULL) {
      # initialized specialized fselect archive and objective
      archive = ArchiveBatchFSelect$new(
        search_space = task_to_domain(assert_task(task)),
        codomain = measures_to_codomain(assert_measures(measures)),
        check_values = check_values)

      objective = ObjectiveFSelectBatch$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
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
    #' The [FSelector] object writes the best found feature subsets and estimated performance values here.
    #' For internal use.
    #'
    #' @param ydt (`data.table::data.table()`)\cr
    #'   Optimal outcomes, e.g. the Pareto front.
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, ydt, ...) {
      # Add feature names to result for easy task subsetting
      features = map(transpose_list(xdt), function(x) {
        self$objective$task$feature_names[as.logical(x)]
      })
      set(xdt, j = "features", value = list(features))
      set(xdt, j = "n_features", value = length(features[[1L]]))
      super$assign_result(xdt, ydt)
      if (!is.null(private$.result$x_domain)) set(private$.result, j = "x_domain", value = NULL)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function(...) {
      catf(format(self))
      catf(str_indent("* State: ", if (is.null(private$.result)) "Not optimized" else "Optimized"))
      catf(str_indent("* Objective:", format(self$objective)))
      catf(str_indent("* Terminator:", format(self$terminator)))
      if (!is.null(private$.result)) {
        catf("* Result:")
        print(self$result[, c(self$archive$cols_x, self$archive$cols_y), with = FALSE])
        catf("* Archive:")
        print(as.data.table(self$archive)[, c(self$archive$cols_x, self$archive$cols_y), with = FALSE])
      }
    }
  ),

  active = list(
    #' @field result_feature_set (list of `character()`)\cr
    #' Feature sets for task subsetting.
    result_feature_set = function() {
      map(self$result$features, function(x) {
        unlist(x)
      })
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
