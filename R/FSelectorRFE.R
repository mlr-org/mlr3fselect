#' @title Feature Selection via Recursive Feature Elimination
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_rfe
#'
#' @description
#' Recursive feature elimination iteratively removes features with a low importance score.
#' Only works with [Learner]s that can calculate importance scores (see section on optional extractors in [Learner]).
#'
#' @details
#' The learner is trained on all features at the start and importance scores are calculated for each feature .
#' Then the least important feature is removed and the learner is trained on the reduced feature set.
#' The importance scores are calculated again and the procedure is repeated until the desired number of features is reached.
#' The non-recursive option (`recursive = FALSE`) only uses the importance scores calculated in the first iteration.
#'
#' The feature selection terminates itself when `n_features` is reached.
#' It is not necessary to set a termination criterion.
#'
#' @templateVar id rfe
#' @template section_dictionary_fselectors
#'
#' @section Parameters:
#' \describe{
#' \item{`n_features`}{`integer(1)`\cr
#'   The number of features to select. By default half of the features are selected.}
#' \item{`feature_fraction`}{`double(1)`\cr
#'   Fraction of features to retain in each iteration, The default 0.5 retrains half of the features.}
#' \item{`feature_number`}{`integer(1)`\cr
#'   Number of features to remove in each iteration.}
#' \item{`subset_sizes`}{`integer()`\cr
#'   Vector of number of features to retain in each iteration.
#'   Must be sorted in decreasing order.}
#' \item{`recursive`}{`logical(1)`\cr
#'   If `TRUE` (default), the feature importance is calculated in each iteration.}
#' }
#'
#' The parameter `feature_fraction`, `feature_number` and `subset_sizes` are mutually exclusive.
#'
#' @export
#' @examples
#' # retrieve task
#' task = tsk("pima")
#'
#' # load learner
#' learner = lrn("classif.rpart")
#'
#' \donttest{
#' # feature selection on the pima indians diabetes data set
#' instance = fselect(
#'   method = "rfe",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   store_models = TRUE
#' )
#'
#' # best performing feature subset
#' instance$result
#'
#' # all evaluated feature subsets
#' as.data.table(instance$archive)
#'
#' # subset the task and fit the final model
#' task$select(instance$result_feature_set)
#' learner$train(task)
#' }
FSelectorRFE = R6Class("FSelectorRFE",
  inherit = FSelector,
  public = list(

    #' @field importance `numeric()`\cr
    #' Stores the feature importance of the model with all variables if `recursive` is set to `FALSE`
    importance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        n_features       = p_int(lower = 1),
        feature_fraction = p_dbl(lower = 0, upper = 1 - 1e-6, default = 0.5),
        feature_number   = p_int(lower = 1),
        subset_sizes     = p_uty(),
        recursive        = p_lgl(default = TRUE)
      )
      ps$values = list(recursive = TRUE, feature_fraction = 0.5)

      super$initialize(
        id = "rfe",
        param_set = ps,
        properties = "single-crit",
        label = "Recursive Feature Elimination",
        man = "mlr3fselect::mlr_fselectors_rfe"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {

      if ("importance" %nin% inst$objective$learner$properties) {
        stopf("%s does not work with %s. Only learners that can calculate importance scores are supported.", format(self), format(inst$objective$learner))
      }

      pars = self$param_set$values
      archive = inst$archive
      feature_names = inst$archive$cols_x
      n = length(feature_names)
      n_features = pars$n_features
      feature_fraction = pars$feature_fraction
      feature_number = pars$feature_number
      subset_sizes = pars$subset_sizes

      if (is.null(n_features)) n_features = floor(n / 2)

      subsets = if (!is.null(feature_number)) {
        seq(from = n - feature_number, to = n_features, by = -feature_number)
      } else if (!is.null(subset_sizes)) {
        subset_sizes
      } else if (!is.null(feature_fraction)) {
        unique(floor(cumprod(c(n, rep(feature_fraction, log(n_features / n) / log(feature_fraction))))))[-1]
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
