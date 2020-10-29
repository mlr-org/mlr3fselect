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
#' \item{`min_features`}{`integer(1)`\cr
#' The minimum number of features to select, default is `1L`.}
#' \item{`feature_fraction`}{`double(1)`\cr
#' Fraction of features to retain in each iteration, default is `0.5`.}
#' \item{`feature_number`}{`integer(1)`\cr
#' Number of features to remove in each iteration.}
#' \item{`subset_sizes`}{`integer()`\cr
#' Vector of number of features to retain in each iteration. Must be sorted in
#' decreasing order.}
#' \item{`recursive`}{`logical(1)`\cr
#' Use the recursive version? Default is `FALSE`.}
#' }
#'
#' The parameter `feature_fraction`, `feature_number` and `subset_sizes` are
#' mutually exclusive.
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
#' \donttest{
#' # Modifies the instance by reference
#' fselector$optimize(instance)
#' 
#' # Returns best scoring evaluation
#' instance$result
#' 
#' # Allows access of data.table of full path of all evaluations
#' instance$archive$data()}
FSelectorRFE = R6Class("FSelectorRFE",
  inherit = FSelector,
  public = list(
    #' @field importance `numeric()`\cr
    #' Stores the feature importance of the model with all variables if
    #' `recrusive` is set to `FALSE`
    importance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("min_features", lower = 1, default = 1),
        ParamDbl$new("feature_fraction", lower = 0,
          upper = 1 - .Machine$double.neg.eps, default = 0.5),
        ParamInt$new("feature_number", lower = 1),
        ParamUty$new("subset_sizes"),
        ParamLgl$new("recursive", default = FALSE))
      )
      ps$values = list(recursive = FALSE, min_features = 1, feature_fraction = 0.5)

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
      n = length(feature_names)
      min_features = pars$min_features
      feature_fraction = pars$feature_fraction
      feature_number = pars$feature_number
      subset_sizes = pars$subset_sizes

      subsets = if (!is.null(feature_number)) {
        seq(from = n - feature_number, to = min_features, by = -feature_number)
      } else if (!is.null(subset_sizes)) {
        subset_sizes
      } else if (!is.null(feature_fraction)) {
        unique(floor(cumprod(c((n), rep(feature_fraction,
          log(min_features / (n)) / log(feature_fraction))))))[-1]
      }

      assert_integerish(rev(subsets), any.missing = FALSE,
        lower = 1, upper = n - 1, sorted = TRUE)

      states = set_names(as.list(rep(TRUE, n)), feature_names)
      states = as.data.table(states)
      inst$eval_batch(states)

      for (i in subsets) {
        if (pars$recursive) {

          # Recalculate the variable importance on the reduced feature subset
          feat = archive$data()[get("batch_nr") == archive$n_batch, feature_names,
            with = FALSE]
          feat = feature_names[as.logical(feat)]

          uhash = archive$data()[get("batch_nr") == archive$n_batch, uhash]
          rr = archive$benchmark_result$resample_result(uhash = uhash)
          learners = extract_learner(rr$learners)

          imp = importance_average(learners, feat)

          # Eliminate the most unimportant feature of the feature subset
          states =
            as.list(feature_names %in% feat & feature_names %in% names(imp[seq(i)]))
          names(states) = feature_names
          states = as.data.table(states)

        } else {
          if (archive$n_batch == 1) {
            # Calculate the variable importance on the complete feature subset
            uhash = archive$data()[get("batch_nr") == 1, uhash]
            rr = archive$benchmark_result$resample_result(uhash = uhash)
            learners = extract_learner(rr$learners)

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

# Extract trained Learners from a list of GraphLeaners
extract_learner = function(graph_learners) {
  map(graph_learners, function(learner) {
    graph = learner$graph
    graph$state = learner$model
    graph$pipeops[[tail(learner$graph$ids(sorted = TRUE), 1)]]$learner_model
  })
}