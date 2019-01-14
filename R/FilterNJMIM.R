#' @title Minimal normalised joint mutual information maximisation
#'
#' @description
#' 	Minimal normalised joint mutual information maximisation filter
#'
#' @section Usage:
#' ```
#' filter = FilterNJMIM$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterNJMIM].
#'
#' @name FilterNJMIM
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterNJMIM$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterNJMIM = R6Class("FilterNJMIM", inherit = Filter,
  public = list(
    initialize = function(id = "FilterNJMIM", settings = list()) {
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
      invoke(praznik::NJMIM, X = X, Y = Y, k = ncol(X), .args = settings)$score
    }
  )
)
