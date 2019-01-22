#' @title AUC
#'
#' @description
#' FilterAUC
#'
#' @section Usage:
#' ```
#' filter = FilterAUC$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterAUC].
#'
#' @name FilterAUC
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterAUC$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterAUC = R6Class("FilterAUC", inherit = Filter,
  public = list(
    initialize = function(id = "FilterAUC", settings = list()) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = "classif",
        settings = settings)
    }
  ),

  private = list(
    .calculate = function(task, settings) {
      score = map_dbl(task$data(col = task$feature_names), function(x, y) {
        measureAUC(x, y, task$negative, task$positive)
      }, y = task$data(col = task$target_names)[[task$target_names]])
      abs(0.5 - score)
    }
  )
)

#' @include mlr_filters.R
mlr_filters$add("FilterAUC", FilterAUC)
