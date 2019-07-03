#' @title Symmetrical Uncertainty Filter
#'
#' @aliases mlr_filters_symmetrical_uncertainty
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description Symmetrical uncertainty filter. Calls
#'   [FSelectorRcpp::information_gain()]. The target variable is automatically
#'   binned (argument `equal` in [FSelectorRcpp::information_gain()]). You might
#'   want to turn this setting of for classification tasks.
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterSymmetricalUncertainty$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterSymmetricalUncertainty = R6Class("FilterSymmetricalUncertainty", inherit = Filter,
  public = list(
    initialize = function(id = "symmetrical_uncertainty", param_vals = list(equal = TRUE)) {
      super$initialize(
        id = id,
        packages = "FSelectorRcpp",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = c("classif", "regr"),
        param_set = ParamSet$new(list(
          ParamLgl$new("equal", default = FALSE, tags = "filter"),
          ParamLgl$new("discIntegers", default = TRUE, tags = "filter"),
          ParamInt$new("threads", lower = 0L, default = 1L, tags = "filter")
        )),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {

      # setting params
      equal = self$param_set$values$equal
      discIntegers = self$param_set$values$discIntegers
      threads = self$param_set$values$threads

      if (is.null(equal)) {
        equal = self$param_set$default$equal
      }
      if (is.null(discIntegers)) {
        discIntegers = self$param_set$default$discIntegers
      }
      if (is.null(threads)) {
        threads = self$param_set$default$threads
      }

      x = as.data.frame(task$data(cols = task$feature_names))
      y = task$truth()

      fv = FSelectorRcpp::information_gain(x = x, y = y, type = "symuncert",
        equal = equal, discIntegers = discIntegers, threads = threads)
      set_names(fv$importance, fv$attributes)
    }
  )
)

register_filter("symmetrical_uncertainty", FilterSymmetricalUncertainty)
