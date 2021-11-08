#' @title Feature Selection via Sequential Selection
#'
#' @description
#' `FSelectorSequential` class that implements sequential feature selection. The
#' sequential forward selection (`strategy = fsf`) extends the feature set in
#' each step with the feature that increases the models performance the most.
#' The sequential backward selection (`strategy = fsb`) starts with the complete
#' future set and removes in each step the feature that decreases the models
#' performance the least.
#'
#' @templateVar id sequential
#' @template section_dictionary_fselectors
#'
#' @section Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#' Maximum number of features. By default, number of features in [mlr3::Task].}
#' \item{`strategy`}{`character(1)`\cr
#' Search method `sfs` (forward search) or `sbs` (backward search).}
#' }
#'
#' @note
#' Feature sets are evaluated in batches, where each batch is one step in the
#' sequential feature selection.
#'
#' @export
#' @template example
FSelectorSequential = R6Class("FSelectorSequential",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.`
    initialize = function() {
      ps = ps(
        max_features = p_int(lower = 1),
        strategy = p_fct(levels = c("sfs", "sbs"), default = "sfs")
      )

      ps$values = list(strategy = "sfs")

      super$initialize(
        param_set = ps, properties = "single-crit"
      )
    },

    #' @description
    #' Returns the optimization path.
    #'
    #' @param inst ([FSelectInstanceSingleCrit])\cr
    #'   Instance optimized with [FSelectorSequential].
    #' @param include_uhash (`logical(1)`)\cr
    #'   Include `uhash` column?
    #'
    #' @return [data.table::data.table()]
    optimization_path = function(inst, include_uhash = FALSE) {
      archive = inst$archive
      if (archive$n_batch == 0L) {
        stop("No results stored in archive")
      }
      uhash = if (include_uhash) "uhash" else NULL
      res = archive$data[, head(.SD, 1), by = get("batch_nr")]
      res[, c(archive$cols_x, archive$cols_y, "batch_nr", uhash), with = FALSE]
    }
  ),
  private = list(
    .optimize = function(inst) {

      pars = self$param_set$values
      archive = inst$archive
      feature_names = inst$archive$cols_x

      if (is.null(pars$max_features)) {
        pars$max_features = length(feature_names)
      }

      # Initialize states for first batch
      if (self$param_set$values$strategy == "sfs") {
        states = as.data.table(diag(TRUE, length(feature_names),
          length(feature_names)))
        names(states) = feature_names
      } else {
        combinations = combn(length(feature_names),
          pars$max_features)
        states = map_dtr(seq_len(ncol(combinations)), function(j) {
          state = rep(FALSE, length(feature_names))
          state[combinations[, j]] = TRUE
          set_names(as.list(state), feature_names)
        })
      }

      inst$eval_batch(states)

      repeat({
        if (archive$n_batch == pars$max_features) break

        res = archive$best(batch = archive$n_batch)
        best_state = as.logical(res[, feature_names, with = FALSE])

        # Generate new states based on best feature set
        x = ifelse(pars$strategy == "sfs", FALSE, TRUE)
        y = ifelse(pars$strategy == "sfs", TRUE, FALSE)
        z = if (pars$strategy == "sfs") !best_state else best_state

        states = map_dtr(seq_along(best_state)[z], function(i) {
          if (best_state[i] == x) {
            new_state = best_state
            new_state[i] = y
            set_names(as.list(new_state), feature_names)
          }
        })

        inst$eval_batch(states)
      })
    }
  )
)

mlr_fselectors$add("sequential", FSelectorSequential)
