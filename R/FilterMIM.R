#' @title Conditional Mutual Information Based Feature Selection Filter
#'
#' @aliases mlr_filters_mim
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description Conditional mutual information based feature selection filter.
#'   Calls [praznik::MIM()].
#'
#' @details This filter supports partial scoring via hyperparameter `k`. To use
#'   it, set `k` during construction via `param_vals.` By default all filter
#'   scores are calculated and the default of `k = 3` in the ParamSet does not
#'   apply.
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterMIM$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterMIM = R6Class("FilterMIM", inherit = Filter,
  public = list(
    initialize = function(id = "mim", param_vals = list()) {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = "classif",
        param_set = ParamSet$new(list(
          ParamInt$new("k", lower = 1L, default = 3L, tags = "filter"),
          ParamInt$new("threads", lower = 0L, default = 0L, tags = "filter"),
          ParamInt$new("nfeat", lower = 1, tags = "generic"),
          ParamDbl$new("frac", lower = 0, upper = 1, tags = "generic"),
          ParamDbl$new("cutoff", tags = "generic")
        )),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {

      # setting params
      k = self$param_set$values$k
      threads = self$param_set$values$threads

      if (is.null(k)) {
        # by default we calculate all scores
        # partial scoring need to be specifically requested by setting k during construction
        k = length(task$feature_names)
      }
      if (is.null(threads)) {
        threads = self$param_set$default$threads
      }

      # n overwrites any param_vals
      if (!is.null(n)) {
        if (!is.null(k)) {
          warningf("Overwriting hyperparameter 'k' with the value given in `$filter_*().")
        }
        k = n
      }

      X = task$data(cols = task$feature_names)
      Y = task$truth()
      praznik::MIM(X = X, Y = Y, k = k, threads = threads)$score
    }
  )
)

register_filter("mim", FilterMIM)
