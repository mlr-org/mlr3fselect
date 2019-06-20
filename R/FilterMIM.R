#' @title Conditional Mutual Information Based Feature Selection Filter
#'
#' @aliases mlr_filters_mim
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include FilterResult.R
#'
#' @description
#' Conditional mutual information based feature selection filter.
#' Calls [praznik::MIM()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterMIM$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterMIM = R6Class("FilterMIM", inherit = FilterResult,
  public = list(
    initialize = function(id = "mim") {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = "classif"
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      X = task$data(cols = task$feature_names)
      Y = task$truth()
      praznik::MIM(X = X, Y = Y, k = ncol(X))$score
    }
  )
)

register_filter("mim", FilterMIM)
