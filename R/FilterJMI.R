#' @title Joint mutual information
#'
#' @description
#' 	Joint mutual information filter
#'
#' @section Usage:
#' ```
#' filter = FilterJMI$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterJMI].
#'
#' @name FilterJMI
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterJMI$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterJMI = R6Class("FilterJMI", inherit = Filter,
  public = list(
    initialize = function(id = "FilterJMI", settings = list()) {
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
      invoke(praznik::JMI, X = X, Y = Y, k = ncol(X), .args = settings)$score
    }
  )
)

#' @include mlr_filters.R
mlr_filters$add("FilterJMI", FilterJMI)
