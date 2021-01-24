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
#' as.data.table(instance$archive)}
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
        unique(floor(cumprod(c(n, rep(feature_fraction, log(min_features / n) / log(feature_fraction))))))[-1]
      }

      assert_integerish(rev(subsets), any.missing = FALSE, lower = 1, upper = n - 1, sorted = TRUE)

      # Create full feature set
      states = set_names(as.list(rep(TRUE, n)), feature_names)
      states = as.data.table(states)
      inst$eval_batch(states)

      # Calculate the variable importance on the full feature set
      uhash = archive$data[get("batch_nr") == 1, uhash]
      rr = archive$benchmark_result$resample_result(uhash = uhash)
      learners = extract_learner(rr$learners)
      imp = importance_average(learners, feature_names)

      # Log importance to archive
      set(archive$data, archive$n_evals, "importance", list(imp))

      for (i in subsets) {
        if (pars$recursive) {

          # Eliminate the most unimportant features
          feat = archive$data[get("batch_nr") == archive$n_batch, feature_names, with = FALSE]
          feat = feature_names[as.logical(feat)]
          states = set_names(as.list(feature_names %in% feat & feature_names %in% names(imp[seq(i)])), feature_names)
          states = as.data.table(states)

          # Fit model on the reduced feature subset
          inst$eval_batch(states)

          # Recalculate the variable importance on the reduced feature subset
          uhash = archive$data[get("batch_nr") == archive$n_batch, uhash]
          rr = archive$benchmark_result$resample_result(uhash = uhash)
          learners = extract_learner(rr$learners)
          feat = feature_names[as.logical(states)]
          imp = importance_average(learners, feat)
          
          # Log importance to archive
          set(archive$data, archive$n_evals, "importance", list(imp))

        } else {
          # Eliminate the most unimportant features
          states = set_names(as.list(feature_names %in% names(imp[seq(i)])), feature_names)
          states = as.data.table(states)

          # Fit model on the reduced feature subset
          inst$eval_batch(states)

          # Log importance to archive
          set(archive$data, archive$n_evals, "importance", list(imp[seq(i)]))
        }
      }
    }
  )
)

# Calculates the average feature importance on all resample iterations.
# Returns a numeric vector of average feature importance in decreasing order.
# Some learners omit features that are not used at all,
# thus we have to assign zero to these features
importance_average = function(learners, features) {
  x = map(learners, function(learner) {
    importance = learner$importance()
    set_names(map_dbl(features, function(feature) {
      if (feature %in% names(importance)) importance[[feature]] else 0
    }), features)
  })

  sort(set_names(pmap_dbl(x, function(...) mean(c(...))), names(x[[1]])), decreasing = TRUE)
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



