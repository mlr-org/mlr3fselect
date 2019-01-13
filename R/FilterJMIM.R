#' @title Minimal joint mutual information maximisation
#'
#' @description
#' 	Minimal joint mutual information maximisation filter
#'
#' @section Usage:
#' ```
#' filter = FilterJMIM$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterJMIM].
#'
#' @name FilterJMIM
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterJMIM$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterJMIM = R6Class("FilterJMIM", inherit = Filter,
  public = list(
    initialize = function(id = "FilterJMIM", settings = list()) {
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
      filter_values = invoke(praznik::JMIM, X = X, Y = Y, k = ncol(X), .args = settings)$score
    }
  )
)
