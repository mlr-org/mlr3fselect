#' @title Double Input Symmetrical Relevance Filter
#'
#' @name mlr_filters_disr
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Double input symmetrical relevance filter.
#' Calls [praznik::DISR()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterDISR$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterDISR = R6Class("FilterDISR", inherit = Filter,
  public = list(
    initialize = function(id = "disr") {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("numeric", "factor", "ordered"),
        task_type = "classif"
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      X = task$data(cols = task$feature_names)
      Y = task$truth()
      invoke(praznik::DISR, X = X, Y = Y, k = ncol(X), .args = self$param_set$values)$score
    }
  )
)
