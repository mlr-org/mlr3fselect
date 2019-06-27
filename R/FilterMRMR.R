#' @title Minimum redundancy maximal relevancy filter
#'
#' @aliases mlr_filters_MRMR
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Minimum redundancy maximal relevancy filter.
#' Calls [praznik::MRMR()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterMRMR$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterMRMR = R6Class("FilterMRMR", inherit = Filter,
  public = list(
    initialize = function(id = "MRMR") {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("numeric", "factor", "integer", "character", "logical"),
        task_type = c("classif", "regr")
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      X = task$data(cols = task$feature_names)
      Y = task$truth()
      praznik::MRMR(X = X, Y = Y, k = ncol(X))$score
    }
  )
)
