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
      ps$values = list(strategy = "sfs")

      super$initialize(
        param_set = ps
      )
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values
      archive = instance$objective$archive
      feature_names = instance$task$feature_names

      if (is.null(pars$max_features)) {
        pars$max_features = length(feature_names)
      }

      # Initialize states for first batch
      if (archive$n_batch == 0) {
        if (self$param_set$values$strategy == "sfs") {
          states = as.data.table(diag(TRUE, length(feature_names),
            length(feature_names)))
          names(states) = feature_names
        } else {
          combinations = combn(length(feature_names),
            pars$max_features)
          states = map_dtr(seq_len(ncol(combinations)), function(j) {
            state = rep(0, length(feature_names))
            state[combinations[, j]] = 1
            state = as.list(as.logical(state))
            names(state) = feature_names
            state
          })
        }
      } else {
        if (archive$n_batch == pars$max_features) {
          stop(terminated_error(instance))
        }

        res = archive$get_best(m = archive$n_batch)
        best_state = as.numeric(as.matrix(res[1, feature_names, with = FALSE]))

        # Generate new states based on best feature set
        x = ifelse(pars$strategy == "sfs", 0, 1)
        y = ifelse(pars$strategy == "sfs", 1, 0)
        z = if (pars$strategy == "sfs") {
          !as.logical(best_state)
        }
        else {
          as.logical(best_state)
        }
        states = map_dtr(seq_along(best_state)[z], function(i) {
          if (best_state[i] == x) {
            new_state = best_state
            new_state[i] = y
            new_state = as.list(as.logical(new_state))
            names(new_state) = feature_names
            new_state
          }
        })
      }
      instance$objective$eval_batch(states)
    }
  )
)

mlr_fselectors$add("sequential", FSelectSequential)
