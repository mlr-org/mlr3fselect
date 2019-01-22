#' @title Minimal conditional mutual information
#'
#' @description
#' 	Minimal conditional mutual information maximisation filter
#'
#' @section Usage:
#' ```
#' filter = FilterCMIM$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterCMIM].
#'
#' @name FilterCMIM
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterCMIM$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterCMIM = R6Class("FilterCMIM", inherit = Filter,
  public = list(
    initialize = function(id = "FilterCMIM", settings = list()) {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("numeric", "factor", "ordered"),
        task_type = c("classif", "regr"),
        settings = settings)
    }
  ),

  private = list(
    .calculate = function(task, settings) {
      X = task$data(cols = task$feature_names)
      Y = task$data(cols = task$target_names)[[task$target_names]]
      invoke(praznik::CMIM, X = X, Y = Y, k = ncol(X), .args = settings)$score
    }
  )
)

#' @include mlr_filters.R
mlr_filters$add("FilterCMIM", FilterCMIM)
