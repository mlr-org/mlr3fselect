#' @title Correlation Filter
#'
#' @aliases mlr_filters_correlation
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description Correlation filter. Calls [stats::cor()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("mtcars")
#' filter = FilterCorrelation$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterCorrelation = R6Class("FilterCorrelation", inherit = Filter,
  public = list(
    initialize = function(id = "correlation", param_vals = list()) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = c("integer", "numeric"),
        task_type = "regr",
        param_set = ParamSet$new(list(
          ParamFct$new("use", default = "everything", levels = c("everything",
            "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"),
          tags = "filter"),
          ParamFct$new("method", default = "pearson",
            levels = c("pearson", "kendall", "spearman"), tags = "filter"),
          ParamInt$new("nfeat", lower = 1, tags = "generic"),
          ParamDbl$new("frac", lower = 0, upper = 1, tags = "generic"),
          ParamDbl$new("cutoff", tags = "generic")
        )),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {

      # setting params
      use = self$param_set$values$use
      method = self$param_set$values$method

      if (is.null(use)) {
        use = self$param_set$default$use
      }
      if (is.null(method)) {
        method = self$param_set$default$method
      }

      fn = task$feature_names
      score = abs(stats::cor(
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = use,
        method = method)[, 1L])
      set_names(score, fn)
    }
  )
)

register_filter("correlation", FilterCorrelation)
