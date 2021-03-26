#' @title Feature Selection via Sequential Selection with Shadow Features
#'
#' @description
#' `FSelectorSequentialPermutation` class that implements a sequential feature selection with permutated shadow features. 
#'
#' @templateVar id sequential_permutation
#' @template section_dictionary_fselectors
#'
#' @note
#' Feature sets are evaluated in batches, where each batch is one step in the
#' sequential feature selection.
#' 
#' @source
#' `r format_bib("thomas2017")`
#' `r format_bib("wu2007")`
#'
#' @export
#' @template example
FSelectorSequentialPermutation = R6Class("FSelectorSequentialPermutation ",
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
      # add shadow variables
      task = inst$objective$task
      data = map_dtc(task$data(cols = task$feature_names), shuffle)
      shadow_variables = sprintf("permuted__%s", colnames(data))
      setnames(data, shadow_variables)
      task$cbind(data)

      # add shadow variables to domain
      old_domain = inst$objective$domain$clone()
      inst$objective$domain = ParamSet$new(map(inst$objective$task$feature_names, function(s) ParamLgl$new(id = s)))
      
      # add shadow variables to search_space
      inst$archive$search_space =  inst$objective$domain
      inst$search_space =  inst$objective$domain

      pars = self$param_set$values
      archive = inst$archive
      feature_names = inst$archive$cols_x

      # initialize states for first batch
      states = as.data.table(diag(TRUE, length(feature_names), length(feature_names)))
      names(states) = feature_names
      
      inst$eval_batch(states) 

      repeat({
        res = archive$best(batch = archive$n_batch)[, feature_names, with = FALSE]

        # check if any shadow variable was selected
        if (any(as.logical(res[, shadow_variables, with = FALSE]))) break

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

      # restore task and domain without shadow variables
      task$select(setdiff(task$feature_names, shadow_variables))
      inst$objective$domain = old_domain
      inst$archive$search_space =  old_domain
      inst$search_space =  old_domain
    },

    .assign_result = function(inst) {
      archive = inst$archive
      # exclude result of last batch since it contains a shadow variables
      if (archive$n_batch == 1) {
        stopf("%s selected a shadow variable in the first iteration.", format(self))
      }
      best = archive$best(seq(archive$n_batch - 1))
      xdt = best[, archive$cols_x, with = FALSE]
      y = unlist(best[, archive$cols_y, with = FALSE])
      inst$assign_result(xdt, y)
    }
  )
)

mlr_fselectors$add("sequential_permutation", FSelectorSequentialPermutation)
