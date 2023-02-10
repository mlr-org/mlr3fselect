#' @title Feature Selection with Recursive Feature Elimination with Cross Validation
#'
#' @include mlr_fselectors.R FSelectorRFE.R
#' @name mlr_fselectors_rfe
#'
#' @description
#' Feature selection using the Recursive Feature Elimination (RFE) algorithm .
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
#'   method = fs("rfe"),
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
FSelectorRFECV = R6Class("FSelectorRFECV",
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
      ps$values = list(recursive = TRUE)

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

      # optimize number of features
      rfe_workhorse(inst, subsets, recursive, folds = resampling_cv$iters)

      # average across iterations
      archive$data[, "iteration" := rep(seq(length(subsets) + 1), resampling_cv$iters )]
      aggr = archive$data[, mean(unlist(.SD)), by = "iteration", .SDcols = archive$cols_y]
      n_features = aggr[order(V1, decreasing = TRUE), head(.SD, 1)]$iter

      # use full data set
      resampling_insample = rsmp("insample")
      resampling_insample$instantiate(inst$objective$task)
      inst$objective$constants$values$resampling = list(resampling_insample)

      # optimize feature set
      subsets = rfe_subsets(n, n_features, feature_number, subset_sizes, feature_fraction)
      rfe_workhorse(inst, subsets, recursive)

      # add iteration
      archive$data[is.na(get("iteration")), "iteration" := seq(length(subsets) + 1)]
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
