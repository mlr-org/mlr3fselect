#' @title Minimal Conditional Mutual Information Filter
#'
#' @aliases mlr_filters_cmim
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Minimal conditional mutual information maximisation filter.
#' Calls [praznik::CMIM()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterCMIM$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterCMIM = R6Class("FilterCMIM", inherit = Filter,
  public = list(
    initialize = function(id = "cmim") {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = c("classif", "regr")
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      X = task$data(cols = task$feature_names)
      Y = task$truth()
      praznik::CMIM(X = X, Y = Y, k = ncol(X))$score
    }
  )
)
