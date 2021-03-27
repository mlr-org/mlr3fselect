#' @title Feature Selection via Sequential Search with Shadow Variables
#'
#' @description
#' `FSelectorShadowVariableSearch` class that implements a sequential search with shadow variables.  
#'
#' @templateVar id shadow_variable_search
#' @template section_dictionary_fselectors
#'
#' @note
#' `FSelectorShadowVariableSearch` terminates itself and should be used with [TerminatorNone].
#' 
#' @source
#' `r format_bib("thomas2017")`
#' `r format_bib("wu2007")`
#'
#' @export
#' @examples
#' library(mlr3)
#' 
#' instance = FSelectInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("none")
#' )
#'
#' fselector = fs("shadow_variable_search")
#' \donttest{
#' # Modifies the instance by reference
#' fselector$optimize(instance)
#'
#' # Returns best scoring evaluation
#' instance$result
#'
#' # Allows access of data.table of full path of all evaluations
#' as.data.table(instance$archive)}
FSelectorShadowVariableSearch = R6Class("FSelectorShadowVariableSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.`
    initialize = function() {
    super$initialize(
        param_set = ps(), properties = "single-crit"
      )
    },

    #' @description
    #' Returns the optimization path.
    #'
    #' @param inst ([FSelectInstanceSingleCrit])\cr
    #' Instance optimized with [FSelectorSequential].
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
      private$.task = task$clone()
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
