#' @title Feature Selection with Random Search
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_random_search
#'
#' @description
#' Feature selection using Random Search Algorithm.
#'
#' @details
#' The feature sets are randomly drawn.
#' The sets are evaluated in batches of size `batch_size`.
#' Larger batches mean we can parallelize more, smaller batches imply a more fine-grained checking of termination criteria.
#'
#' @templateVar id random_search
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#' Maximum number of features.
#' By default, number of features in [mlr3::Task].}
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of feature sets to try in a batch.}
#' }
#'
#' @source
#' `r format_bib("bergstra_2012")`
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
#'   fselector = fs("random_search"),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 10
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
FSelectorRandomSearch = R6Class("FSelectorRandomSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        "max_features" = p_int(lower = 1),
        "batch_size" = p_int(default = 1, lower = 1)
      )

      ps$values = list(batch_size = 1L)

      super$initialize(
        id = "random_search",
        param_set = ps,
        properties = c("single-crit", "multi-crit"),
        label = "Random Search",
        man = "mlr3fselect::mlr_fselectors_random_search"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      feature_names = inst$archive$cols_x
      max_features = pars$max_features %??% length(feature_names)

      repeat {
        X = t(replicate(pars$batch_size, {
          n = sample.int(max_features, 1L)
          x = sample.int(length(feature_names), n)
          replace(logical(length(feature_names)), x, TRUE)
        }))
        colnames(X) = feature_names
        inst$eval_batch(as.data.table(X))
      }
    }
  )
)

mlr_fselectors$add("random_search", FSelectorRandomSearch)
