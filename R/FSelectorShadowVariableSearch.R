#' @title Feature Selection with Shadow Variable Search
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_shadow_variable_search
#'
#' @description
#' Feature selection using the Shadow Variable Search Algorithm.
#' Shadow variable search creates for each feature a permutated copy and stops when one of them is selected.
#'
#' @details
#' The feature selection terminates itself when the first shadow variable is selected.
#' It is not necessary to set a termination criterion.
#'
#' @section Resources:
#' The [gallery](https://mlr-org.com/gallery.html) features a collection of case studies and demos about optimization.
#'
#' * Run a feature selection with [Shadow Variable Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).
#'
#' @templateVar id shadow_variable_search
#' @template section_dictionary_fselectors
#'
#' @source
#' `r format_bib("thomas2017", "wu2007")`
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
#'   fselector = fs("shadow_variable_search"),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
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
FSelectorShadowVariableSearch = R6Class("FSelectorShadowVariableSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.`
    initialize = function() {
    super$initialize(
      id = "shadow_variable_search",
      param_set = ps(),
      properties = "single-crit",
      label = "Shadow Variable Search",
      man = "mlr3fselect::mlr_fselectors_shadow_variable_search"
    )
    },

    #' @description
    #' Returns the optimization path.
    #'
    #' @param inst ([FSelectInstanceSingleCrit])\cr
    #' Instance optimized with [FSelectorShadowVariableSearch].
    #'
    #' @return [data.table::data.table]
    optimization_path = function(inst) {
      if (inst$archive$n_batch == 0L) {
        stop("No results stored in archive")
      }

      # we have to use the best method to get the same tie breaking as in the optimize method
      map_dtr(seq(inst$archive$n_batch), function(n) {
        inst$archive$best(batch = n)
      })
    }
  ),

  private = list(
    .optimize = function(inst) {

      # save initial state
      task = inst$objective$task
      private$.task = task$clone(deep = TRUE)
      private$.domain = inst$objective$domain$clone()

       # add shadow variables to task
      data = map_dtc(task$data(cols = task$feature_names), shuffle)
      shadow_variables = sprintf("permuted__%s", colnames(data))
      setnames(data, shadow_variables)
      task$cbind(data)

      # add shadow variables to domain
      inst$objective$domain = ParamSet$new(map(inst$objective$task$feature_names, function(s) ParamLgl$new(id = s)))

      # add shadow variables to search_space
      inst$archive$search_space =  inst$objective$domain
      inst$search_space =  inst$objective$domain

      pars = self$param_set$values
      archive = inst$archive
      feature_names = inst$archive$cols_x

      # initialize states for first batch
      states = set_names(as.data.table(diag(TRUE, length(feature_names), length(feature_names))), feature_names)

      inst$eval_batch(states)

      repeat({
        res = archive$best(batch = archive$n_batch)[, feature_names, with = FALSE]

        # check if any shadow variable was selected
        if (any(as.logical(res[, shadow_variables, with = FALSE]))) {

          # stop if the first selected feature is a shadow variable
          if (archive$n_batch == 1) stop("The first selected feature is a shadow variable.")

          # remove last batch with selected shadow variable from archive
          archive = inst$archive
          archive$data = archive$data[get("batch_nr") != archive$n_batch, ]
          break
        }

        best_state = as.logical(res)
        states = map_dtr(seq_along(best_state)[!best_state], function(i) {
          if (!best_state[i]) {
            new_state = best_state
            new_state[i] = TRUE
            set_names(as.list(new_state), feature_names)
          }
        })
        inst$eval_batch(states)
      })
    },

    .assign_result = function(inst) {
      # restore task and domain without shadow variables
      inst$objective$task = private$.task
      inst$objective$domain = private$.domain
      inst$archive$search_space = private$.domain
      inst$search_space = private$.domain

      assign_result_default(inst)
    },

    .task = NULL,

    .domain = NULL
  )
)

mlr_fselectors$add("shadow_variable_search", FSelectorShadowVariableSearch)
