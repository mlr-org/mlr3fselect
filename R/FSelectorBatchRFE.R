#' @title Feature Selection with Recursive Feature Elimination
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_rfe
#'
#' @description
#' Feature selection using the Recursive Feature Elimination (RFE) algorithm.
#' Recursive feature elimination iteratively removes features with a low importance score.
#' Only works with [mlr3::Learner]s that can calculate importance scores (see the section on optional extractors in [mlr3::Learner]).
#'
#' @details
#' The learner is trained on all features at the start and importance scores are calculated for each feature.
#' Then the least important feature is removed and the learner is trained on the reduced feature set.
#' The importance scores are calculated again and the procedure is repeated until the desired number of features is reached.
#' The non-recursive option (`recursive = FALSE`) only uses the importance scores calculated in the first iteration.
#'
#' The feature selection terminates itself when `n_features` is reached.
#' It is not necessary to set a termination criterion.
#'
#' When using a cross-validation resampling strategy, the importance scores of the resampling iterations are aggregated.
#' The parameter `aggregation` determines how the importance scores are aggregated.
#' By default (`"rank"`), the importance score vector of each fold is ranked and the feature with the lowest average rank is removed.
#' The option `"mean"` averages the score of each feature across the resampling iterations and removes the feature with the lowest average score.
#' Averaging the scores is not appropriate for most importance measures.
#'
#' @section Archive:
#' The [ArchiveBatchFSelect] holds the following additional columns:
#'  * `"importance"` (`numeric()`)\cr
#'    The importance score vector of the feature subset.
#'
#' @section Resources:
#' The [gallery](https://mlr-org.com/gallery.html) features a collection of case studies and demos about optimization.
#'
#' * Utilize the built-in feature importance of models with [Recursive Feature Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).
#'
#' @templateVar id rfe
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' \describe{
#' \item{`n_features`}{`integer(1)`\cr
#'   The minimum number of features to select, by default half of the features.}
#' \item{`feature_fraction`}{`double(1)`\cr
#'   Fraction of features to retain in each iteration.
#'   The default of 0.5 retains half of the features.}
#' \item{`feature_number`}{`integer(1)`\cr
#'   Number of features to remove in each iteration.}
#' \item{`subset_sizes`}{`integer()`\cr
#'   Vector of the number of features to retain in each iteration.
#'   Must be sorted in decreasing order.}
#' \item{`recursive`}{`logical(1)`\cr
#'   If `TRUE` (default), the feature importance is calculated in each iteration.}
#' \item{`aggregation`}{`character(1)`\cr
#'   The aggregation method for the importance scores of the resampling iterations.
#'   See details.
#'   }
#' }
#'
#' The parameter `feature_fraction`, `feature_number` and `subset_sizes` are mutually exclusive.
#'
#' @source
#' `r format_bib("guyon2002")`
#'
#' @family FSelector
#' @export
#' @examples
#' # Feature Selection
#' \donttest{
#'
#' # retrieve task and load learner
#' task = tsk("penguins")
#' learner = lrn("classif.rpart")
#'
#' # run feature selection on the Palmer Penguins data set
#' instance = fselect(
#'   fselector = fs("rfe"),
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
FSelectorBatchRFE = R6Class("FSelectorBatchRFE",
  inherit = FSelectorBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        n_features       = p_int(lower = 1),
        feature_fraction = p_dbl(lower = 0, upper = 1 - 1e-6, default = 0.5),
        feature_number   = p_int(lower = 1),
        subset_sizes     = p_uty(),
        recursive        = p_lgl(default = TRUE),
        aggregation      = p_fct(c("mean", "rank"), default = "rank")
      )
      ps$values = list(recursive = TRUE, aggregation = "rank")

      super$initialize(
        id = "rfe",
        param_set = ps,
        properties = c("single-crit", "requires_model"),
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
      recursive = pars$recursive
      aggregation = switch(pars$aggregation,
        "mean" = average_importance,
        "rank" = rank_importance
      )

      if (is.null(n_features)) n_features = floor(n / 2)

      if (is.null(feature_fraction) && is.null(feature_number) && is.null(subset_sizes)) {
        feature_fraction = 0.5
      }

      if (sum(!is.null(feature_fraction), !is.null(feature_number), !is.null(subset_sizes)) > 1) {
        stopf("Only one of `feature_fraction`, `feature_number` and `subset_sizes` can be set.")
      }

      subsets = rfe_subsets(n, n_features, feature_number, subset_sizes, feature_fraction)
      rfe_workhorse(inst, subsets, recursive, aggregation)
    },

    .assign_result = function(inst) {
      assert_class(inst, "FSelectInstanceBatchSingleCrit")
      res = inst$archive$best()

      xdt = res[, c(inst$search_space$ids(), "importance"), with = FALSE]

      # unlist keeps name!
      y = unlist(res[, inst$archive$cols_y, with = FALSE])
      inst$assign_result(xdt, y)

      invisible(NULL)
    }
  )
)


# Some learners omit features that are not used at all, thus we have to assign zero to these features
fix_importance = function(learners, features) {
  map(learners, function(learner) {
    importance = learner$base_learner()$importance()
    set_names(map_dbl(features, function(feature) {
      if (feature %in% names(importance)) importance[[feature]] else 0
    }), features)
  })
}

# Returns the importance of a resampling with one iteration.
raw_importance = function(learners, features) {
  sort(fix_importance(learners, features)[[1]], decreasing = TRUE)
}

# Calculates the average feature importance on all resample iterations.
# Returns a numeric vector of average feature importance in decreasing order.
average_importance = function(learners, features) {
  importances = fix_importance(learners, features)
  sort(set_names(pmap_dbl(importances, function(...) mean(c(...))), names(importances[[1]])), decreasing = TRUE)
}

# Rank the features by their importance and average the ranking across all resample iterations.
# Returns a numeric vector of the average rank in decreasing order.
rank_importance = function(learners, features) {
  importances = fix_importance(learners, features)
  ranked_importances = map(importances, function(importance) rank(importance, ties.method = "random"))
  sort(set_names(pmap_dbl(ranked_importances, function(...) mean(c(...))), names(importances[[1]])), decreasing = TRUE)
}

# Returns the sizes of the feature subsets
rfe_subsets = function(n, n_features, feature_number, subset_sizes, feature_fraction) {

  subsets = if (!is.null(feature_number)) {
    seq(from = n, to = n_features, by = -feature_number)
  } else if (!is.null(subset_sizes)) {
    if (subset_sizes[1] != n) subset_sizes = c(n, subset_sizes)
    subset_sizes
  } else if (!is.null(feature_fraction)) {
    unique(floor(cumprod(c(n, rep(feature_fraction, log(n_features / n) / log(feature_fraction))))))
  }

  assert_integerish(rev(subsets), any.missing = FALSE, lower = 1, upper = n, sorted = TRUE)
  subsets
}

# Run recursive feature elimination
# instance is changed by reference
rfe_workhorse = function(inst, subsets, recursive, aggregation = raw_importance, folds = 1) {
  archive = inst$archive
  feature_names = inst$archive$cols_x
  n = length(feature_names)

  # Create full feature set
  states = set_names(as.data.table(matrix(TRUE, nrow = folds, ncol = n)), feature_names)
  states = as.data.table(states)
  inst$eval_batch(states)

  # Calculate the variable importance on the full feature set
  uhashes = archive$data[list(archive$n_batch), "uhash", on = "batch_nr"][[1]]
  importances = map(uhashes, function(uhash) {
    rr = archive$benchmark_result$resample_result(uhash = uhash)
    aggregation(rr$learners, rr$task$feature_names)
  })

  # discard models if requested by the user
  if (!inst$objective$store_models) inst$archive$benchmark_result$discard(models = TRUE)

  # Log importance and fold to archive
  archive$data[list(archive$n_batch), "importance" := importances, on = "batch_nr"]

  for (j in subsets[-1]) {
    # eliminate features with the lowest importance
    states = archive$data[list(archive$n_batch), archive$cols_x, on = "batch_nr", with = FALSE]
    iwalk(importances, function(importance, i) {
      importance = names(importance[-seq(j)])
      set(states, i, importance, FALSE)
    })

    # fit model on the reduced feature subsets
    inst$eval_batch(states)

    if (recursive) {
      # recalculate the variable importance on the reduced feature subset
      uhashes = archive$data[list(archive$n_batch), "uhash", on = "batch_nr"][[1]]
      importances = map(uhashes, function(uhash) {
        rr = archive$benchmark_result$resample_result(uhash = uhash)
        aggregation(rr$learners, rr$task$feature_names)
      })

      # log importance to archive
      archive$data[list(archive$n_batch), "importance" := importances, on = "batch_nr"]
    } else {
      # log importance to archive
      set(archive$data, archive$n_evals, "importance", map(importances, function(x) x[seq(j)]))
    }
  }
  if (folds > 1) set(archive$data, j = "iteration", value = rep(seq(folds), length(subsets)))

  # discard models if requested by the user
  if (!inst$objective$store_models) inst$archive$benchmark_result$discard(models = TRUE)
}

mlr_fselectors$add("rfe", FSelectorBatchRFE)
