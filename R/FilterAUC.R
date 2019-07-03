#' @title AUC Filter
#'
#' @aliases mlr_filters_auc
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#' @include mlr_filters.R
#'
#' @description Area under the (ROC) Curve filter. Calls [Metrics::auc()], and
#'   scores with the absolute value of the difference between the AUC and 0.5.
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterAUC$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterAUC = R6Class("FilterAUC", inherit = Filter,
  public = list(
    initialize = function(id = "auc", param_vals = list()) {
      super$initialize(
        id = id,
        packages = "Metrics",
        feature_types = c("integer", "numeric"),
        task_type = "classif",
        task_properties = "twoclass",
        param_set = ParamSet$new(),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {
      x = task$truth() == task$positive
      y = task$data(cols = task$feature_names)
      score = map_dbl(y, function(y) Metrics::auc(x, y))
      abs(0.5 - score)
    }
  )
)

register_filter("auc", FilterAUC)
