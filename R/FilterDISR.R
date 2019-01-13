#' @title Double input symmetrical relevance
#'
#' @description
#' 	Double input symmetrical relevance filter
#'
#' @section Usage:
#' ```
#' filter = FilterDISR$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterDISR].
#'
#' @name FilterDISR
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterDISR$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterDISR = R6Class("FilterDISR", inherit = Filter,
  public = list(
    initialize = function(id = "FilterDISR", settings = list()) {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("numeric", "factor", "ordered"),
        task_type = "classif",
        settings = settings)
    }
  ),

  private = list(
    .calculate = function(task, settings) {
      X = task$data(cols = task$feature_names)
      Y = task$data(cols = task$target_names)[[task$target_names]]
      invoke(praznik::DISR, X = X, Y = Y, k = ncol(X), .args = settings)$score
    }
  )
)

