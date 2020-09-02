#' @title Feature Selection via Recursive Feature Elimination
#'
#' @description
#' `FSelectorRFE` class that implements Recursive Feature Elimination (RFE). The
#' recursive algorithm (`recursive = TRUE`) recomputes the feature importance
#' on the reduced feature set in every iteration.  The non-recursive algorithm
#' (`recursive = FALSE`) only uses the feature importance of the model fitted
#' with all features to eliminate the next most unimportant features in every
#' iteration.
#'
#' @templateVar id rfe
#' @template section_dictionary_fselectors
#'
#' @section Parameters:
#' \describe{
#' \item{`subset_size`}{`integer()`\cr
#' Number of features to retain in each iteration.}
#' \item{`recursive`}{`logical(1)`}
#' }
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' terminator = trm("evals", n_evals = 10)
#' instance = FSelectInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator,
#'   store_models = TRUE
#' )
#'
#' fselector = fs("rfe")
#' fselector$optimize(instance)
#' instance$result
#' instance$archive$data
FSelectorRFE = R6Class("FSelectorRFE",
  inherit = FSelector,
  public = list(
    #' @field importance Stores the feature importance of the model with all
    #' variables if `recrusive` is set to `FALSE`
    importance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("subset_sizes"),
        ParamLgl$new("recursive", default = FALSE))
      )
      ps$values = list(recursive = FALSE)

      super$initialize(
        param_set = ps, properties = "single-crit"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {

      pars = self$param_set$values
      archive = inst$archive
      feature_names = inst$archive$cols_x
      if (is.null(pars$subset_sizes)) {
        pars$subset_sizes = rev(seq(length(feature_names) - 1))
      }
      assert_integerish(rev(pars$subset_sizes), any.missing = FALSE,
        lower = 1, upper = length(feature_names) - 1, sorted = TRUE)

      states = set_names(as.list(rep(TRUE, length(feature_names))), feature_names)
      states = as.data.table(states)
      inst$eval_batch(states)

      for (i in pars$subset_sizes) {
        if (pars$recursive) {

          # Recalculate the variable importance on the reduced feature subset
          feat = archive$data()[batch_nr == archive$n_batch, feature_names,
            with = FALSE]
          feat = feature_names[as.logical(feat)]

          rr = archive$data()[batch_nr == archive$n_batch, resample_result][[1]]
          learners = rr$learners
          imp = importance_average(learners, feat)

          # Eliminate the most unimportant feature of the feature subset
          states =
            as.list(feature_names %in% feat & feature_names %in% names(imp[seq(i)]))
          names(states) = feature_names
          states = as.data.table(states)

        } else {
          if (archive$n_batch == 1) {
            # Calculate the variable importance on the complete feature subset
            rr = archive$data()[batch_nr == archive$n_batch, resample_result][[1]]
            learners = rr$learners

            self$importance = importance_average(learners, feature_names)
          }
          # Eliminate the most unimportant features
          states = as.list(feature_names %in% names(self$importance[seq(i)]))
          names(states) = feature_names
          states = as.data.table(states)
        }
        # Fit the model on the reduced feature subset
        inst$eval_batch(states)
      }
    }
  )
)

# Calculates the average feature importance on all resample iterations.
# Returns a numeric vector of average feature importance in decreasing order.
# Some learners omit features that are not used at all,
# thus we have to assign zero to these features
importance_average = function(learners, features) {
  imp = sapply(learners, function(x) {
    imp_r = x$importance()
    sapply(features, function(y) {
      if (y %in% names(imp_r)) imp_r[[y]] else 0
    })
  })
  sort(apply(imp, 1, mean), decreasing = TRUE)
}

mlr_fselectors$add("rfe", FSelectorRFE)
