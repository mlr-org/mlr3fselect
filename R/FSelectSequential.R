#' @title FSelectSequential
#'
#' @description
#' Subclass for sequential feature selection. The sequential forward selection
#' (`strategy = fsf`) extends the feature set in each step with the feature that
#' increases the models performance the most. The sequential backward selection
#' (`strategy = fsb`) starts with the complete future set and removes in each
#' step the feature that decreases the models performance the least.
#'
#' @section Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`
#' Maximum number of features. By default, number of features in [mlr3::Task].}
#' \item{`strategy`}{`character(1)`
#' Search method `sfs` (forward search) or `sbs` (backward search).}
#' }
#'
#' @note
#' Feature sets are evaluated in batches, where each batch is one step in the
#' sequential feature selection.
#'
#' @export
#' @templateVar fs "sequential"
#' @template example
FSelectSequential = R6Class("FSelectSequential",
  inherit = FSelect,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.`
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("max_features", lower = 1),
        ParamFct$new("strategy", levels = c("sfs", "sbs"), default = "sfs"))
      )

      super$initialize(
        param_set = ps
      )
      if (is.null(self$param_set$values$strategy)) {
        self$param_set$values = insert_named(self$param_set$values,
          list(strategy = "sfs"))
      }
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values
      if (is.null(pars$max_features)) {
        pars$max_features = length(instance$task$feature_names)
      }

      # Initialize states for first batch
      if (instance$n_batch == 0) {
        if (self$param_set$values$strategy == "sfs") {
          states = diag(1, length(instance$task$feature_names),
            length(instance$task$feature_names))
        } else {
          combinations = combn(length(instance$task$feature_names),
            pars$max_features)
          states = t(sapply(seq_len(ncol(combinations)), function(j) {
            state = rep(0, length(instance$task$feature_names))
            state[combinations[, j]] = 1
            state
          }))
        }
      } else {
        if (instance$n_batch == pars$max_features) {
          stop(terminated_error(instance))
        }

        # Query bmr for best feature subset of last batch
        rr = instance$best(m = instance$n_batch)
        feat = instance$bmr$rr_data[rr$uhash, on = "uhash"]$feat[[1]]
        best_state = as.numeric(instance$task$feature_names %in% feat)

        # Generate new states based on best feature set
        x = ifelse(pars$strategy == "sfs", 0, 1)
        y = ifelse(pars$strategy == "sfs", 1, 0)
        z = if (pars$strategy == "sfs") {
          !as.logical(best_state)
        }
        else {
          as.logical(best_state)
        }
        states = t(sapply(seq_along(best_state)[z], function(i) {
          if (best_state[i] == x) {
            new_state = best_state
            new_state[i] = y
            new_state
          }
        }))
      }
      instance$eval_batch(states)
    }
  )
)

mlr_fselectors$add("sequential", FSelectSequential)
