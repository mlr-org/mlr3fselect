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
      ps$values = list(min_features = 1L, recursive = FALSE)

      super$initialize(
        param_set = ps
      )
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values
      archive = instance$evaluator$archive
      feature_names = instance$task$feature_names

      if (archive$n_batch == 0L) {
        instance$store_models = TRUE
        states = as.list(rep(TRUE, length(feature_names)))
        names(states) = feature_names
        states = as.data.table(states)
      } else {
        if (length(feature_names) - archive$n_batch < pars$min_features) {
          stop(terminated_error(instance))
        }

        if (pars$recursive) {
          # Recalculate the variable importance on the reduced feature subset
          feat = archive$data[batch_nr == archive$n_batch, feature_names, with = FALSE]
          feat = feature_names[as.logical(feat)]

          bmr_data = archive$data[batch_nr == archive$n_batch, bmr_data][[1]]
          learners = bmr_data$learner
          imp = importance_average(learners, feat)

          # Eliminate the most unimportant feature of the feature subset
          states =
            as.list(feature_names %in% feat & !feature_names %in% names(imp[1]))
          names(states) = feature_names
          states = as.data.table(states)

        } else {
          if (archive$n_batch == 1) {
            # Calculate the variable importance on the complete feature subset
            bmr_data = archive$data[batch_nr == archive$n_batch, bmr_data][[1]]
            learners = bmr_data$learner

            self$importance = importance_average(learners, feature_names)
          }
          # Eliminate the most unimportant features
          states =
            as.list(!feature_names %in% names(self$importance[1:archive$n_batch]))
          names(states) = instance$task$feature_names
          states = as.data.table(states)
        }
      }
      # Fit the model on the reduced feature subset
      instance$evaluator$eval_batch(states)
    }
  )
)

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

mlr_fselectors$add("rfe", FSelectRFE)
