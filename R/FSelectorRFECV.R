#' @title Feature Selection with Recursive Feature Elimination with Cross Validation
#'
#' @include mlr_fselectors.R FSelectorRFE.R
#' @name mlr_fselectors_rfecv
#'
#' @description
#' Feature selection using the Recursive Feature Elimination with Cross-Validation (RFE-CV) algorithm.
#' See [FSelectorRFE] for a description of the base algorithm.
#' RFE-CV runs a recursive feature elimination in each iteration of a cross-validation to determine the optimal number of features.
#' Then a recursive feature elimination is run again on the complete dataset with the optimal number of features as the final feature set size.
#' The performance of the optimal feature set is calculated on the complete data set and should not be reported as the performance of the final model.
#' Only works with [mlr3::Learner]s that can calculate importance scores (see the section on optional extractors in [mlr3::Learner]).
#'
#' @details
#' The resampling strategy is changed during the feature selection.
#' The resampling strategy passed to the instance (`resampling`) is used to determine the optimal number of features.
#' Usually, a cross-validation strategy is used and a recursive feature elimination is run in each iteration of the cross-validation.
#' Internally, [mlr3::ResamplingCustom] is used to emulate this part of the algorithm.
#' In the final recursive feature elimination run the resampling strategy is changed to [mlr3::ResamplingInsample] i.e. the complete data set is used for training and testing.
#'
#' The feature selection terminates itself when the optimal number of features is reached.
#' It is not necessary to set a termination criterion.
#'
#' @section Archive:
#' The [ArchiveFSelect] holds the following additional columns:
#'  * `"iteration"` (`integer(1)`)\cr
#'    The resampling iteration in which the feature subset was evaluated.
#'  * `"importance"` (`numeric()`)\cr
#'    The importance score vector of the feature subset.
#'
#' @templateVar id rfe
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' \describe{
#' \item{`n_features`}{`integer(1)`\cr
#'   The number of features to select.
#'   By default half of the features are selected.}
#' \item{`feature_fraction`}{`double(1)`\cr
#'   Fraction of features to retain in each iteration.
#'   The default 0.5 retrains half of the features.}
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
#'   fselector = fs("rfecv"),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
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
FSelectorRFECV = R6Class("FSelectorRFECV",
  inherit = FSelector,
  public = list(

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
      ps$values = list(recursive = TRUE)

      super$initialize(
        id = "rfecv",
        param_set = ps,
        properties = c("single-crit", "requires_model"),
        label = "Recursive Feature Elimination",
        man = "mlr3fselect::mlr_fselectors_rfecv"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {

      if ("importance" %nin% inst$objective$learner$properties) {
        stopf("%s does not work with %s. Only learners that can calculate importance scores are supported.", format(self), format(inst$objective$learner))
      }

      pars = self$param_set$values
      n = length(inst$archive$cols_x)
      n_features = pars$n_features
      feature_fraction = pars$feature_fraction
      feature_number = pars$feature_number
      subset_sizes = pars$subset_sizes
      recursive = pars$recursive
      archive = inst$archive

      if (is.null(n_features)) n_features = floor(n / 2)

      if (is.null(feature_fraction) && is.null(feature_number) && is.null(subset_sizes)) {
        feature_fraction = 0.5
      }

      if (sum(!is.null(feature_fraction), !is.null(feature_number), !is.null(subset_sizes)) > 1) {
        stopf("Only one of `feature_fraction`, `feature_number` and `subset_sizes` can be set.")
      }

      subsets = rfe_subsets(n, n_features, feature_number, subset_sizes, feature_fraction)

      resampling_cv = inst$objective$resampling$clone()

      inst$objective$constants$values$resampling = map(seq(resampling_cv$iters), function(i) {
        custom = rsmp("custom")
        train_sets = list(resampling_cv$train_set(i))
        test_sets = list(resampling_cv$test_set(i))
        custom$instantiate(inst$objective$task, train_sets, test_sets)
      })

      # optimize the number of features
      rfe_workhorse(inst, subsets, recursive, folds = resampling_cv$iters)

      # average performance of feature numbers
      aggr = archive$data[, list("y" = mean(unlist(.SD))), by = "batch_nr", .SDcols = archive$cols_y]
      best_batch = aggr[order(get("y"), decreasing = TRUE), head(.SD, 1)]$batch_nr
      n_features = rowSums(archive$data[list(best_batch), , on = "batch_nr"][1, archive$cols_x, with = FALSE])

      # use full data set
      resampling_insample = rsmp("insample")
      resampling_insample$instantiate(inst$objective$task)
      inst$objective$constants$values$resampling = list(resampling_insample)

      # optimize feature set
      subsets = rfe_subsets(n, n_features, feature_number, subset_sizes, feature_fraction)
      rfe_workhorse(inst, subsets, recursive)
    },


    .assign_result = function(inst) {
      assert_multi_class(inst, c("FSelectInstanceSingleCrit", "FSelectInstanceMultiCrit"))

      xdt = inst$archive$data[inst$archive$n_evals, inst$search_space$ids(), with = FALSE]
      y = unlist(inst$archive$data[inst$archive$n_evals, inst$archive$cols_y, with = FALSE])

      inst$assign_result(xdt, y)
    }
  )
)

mlr_fselectors$add("rfecv", FSelectorRFECV)
