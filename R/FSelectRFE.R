#' @title FSelectRFE Class
#'
#' @description
#' Class for feature selection by recursive feature elimination (RFE). The
#' recursive algorithm (`recursive = TRUE`) recomputes the feature importances
#' on the reduced feature set in every iteration.  The non-recursive algorithm
#' (`recursive = FALSE`) only uses the feature importances of the model fitted
#' with all features to eliminate the next most unimportant feature in every
#' iteration.
#'
#' @section Parameters:
#' \describe{
#' \item{`min_features`}{`integer(1)`
#' Minimum number of features. By default, 1.}
#' \item{`recursive`}{`logical(1)`}
#' }
#'
#' @export
#' @templateVar fs "rfe"
#' @template example
FSelectRFE = R6Class("FSelectRFE",
  inherit = FSelect,
  public = list(
    #' @field importance Stores the feature importance of the model with all
    #'   variables if `recrusive` is set to `FALSE`
    importance = NULL,

    #' @description
    #' Create new `FSelectRFE` object.
    #' @return `FSelectRFE`
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("min_features", lower = 1),
        ParamLgl$new("recursive", default = FALSE))
      )

      super$initialize(
        param_set = ps
      )
      if (is.null(self$param_set$values$min_features)) {
        self$param_set$values = insert_named(self$param_set$values, list(min_features = 1))
      }
      if (is.null(self$param_set$values$recursive)) {
        self$param_set$values = insert_named(self$param_set$values, list(recursive = FALSE))
      }
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values

      if (instance$n_batch == 0) {
        instance$bm_args = insert_named(instance$bm_args, list(store_models = TRUE))
        states = matrix(1, nrow = 1, ncol = length(instance$task$feature_names))
      } else {
        if (length(instance$task$feature_names) - instance$n_batch < pars$min_features) {
          stop(terminated_error(instance))
        }
        if (pars$recursive) {
          # Recalculate the variable importance on the reduced feature subset
          id = instance$bmr$rr_data[batch_nr == instance$n_batch, uhash]
          feat = instance$bmr$rr_data[uhash == id, feat][[1]]
          learners = instance$bmr$data[uhash == id, learner]
          imp = importance_average(learners, feat)

          # Eliminate the most unimportant feature of the feature subset
          states = as.numeric(instance$task$feature_names %in% feat & !instance$task$feature_names %in% names(imp[1]))
        } else {
          if (instance$n_batch == 1) {
            # Calculate the variable importance on the complete feature subset
            self$importance = importance_average(instance$bmr$data$learner, instance$task$feature_names)
          }

          # Eliminate the most unimportant features
          states = as.numeric(!instance$task$feature_names %in% names(self$importance[1:instance$n_batch]))
        }
      }
      # Fit the model on the reduced feature subset
      states = matrix(states, ncol = length(instance$task$feature_names))
      instance$eval_batch(states)
    }
  )
)

mlr_fselectors$add("rfe", FSelectRFE)

# Calculates the average feature importances on all resample iterations.
# Returns a numeric vector of average feature importances in ascending order.
# Some learners omit features that are not used at all,
# thus we have to assign zero to these features
importance_average = function(learners, features) {
  imp = sapply(learners, function(x) {
    imp_r = x$importance()
    sapply(features, function(y) {
      if (y %in% names(imp_r)) imp_r[[y]] else 0
    })
  })
  sort(apply(imp, 1, mean))
}
