#' @title Filter Base Class
#'
#' @description
#' FilterBase.
#'
#' @section Usage:
#' ```
#' Filter = Filter$new(id)
#' # public members
#' filter$id
#' filter$ff
#' filter$settings
#' # public methods
#' filter$tune()
#' filter$tune_result()
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   The id of the Filter
#' * `settings` (`list`):\cr
#'   The settings for the Filter
#' * `packages` (`character()`]:\cr
#'   Set of required packages.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Filter].
#' * `$id()` stores an identifier for this [Filter].
#' * `$settings()` is a list of hyperparamter settings for this [Filter].
#' * `$packages` (`character()`) stores the names of required packages.
#' * `$createFilterValuesData()` calculates the filter values
#' * `$filterFeatures()` filters a [Task] using specific criteria
#'     -  returns a subsetted [Task]
#' @name Filter
#' @family Filter
NULL

#' @export
Filter = R6Class("Filter",
                 public = list(
                   id = NULL,
                   settings = NULL,
                   packages = NULL,
                   task = NULL,

                   initialize = function(id, settings = list(), packages, task) {
                     self$id = assert_string(id)
                     self$settings = assert_list(settings, names = "unique")
                     self$packages = assert_string(packages)
                     self$task = mlr3:::assert_task(task)

                     # ff$hooks$update_start = c(ff$hooks$update_start, list(terminator$update_start))
                     # ff$hooks$update_end = c(ff$hooks$update_end, list(terminator$update_end))
                   },
                   createFilterValuesData = function(task) {
                     browser()
                     # calls the createFilterValuesData_step() method of each filter
                     private$createFilterValuesData_step(task)
                     invisible(self)
                   }

                   # tune = function() {
                   #   while (!self$terminator$terminated) {
                   #     private$tune_step()
                   #   }
                   #   invisible(self)
                   # },
                   #
                   # tune_result = function() {
                   #   measure = self$ff$measures[[1L]]
                   #   rr = self$ff$bmr$get_best(measure)
                   #   list(performance = rr$aggregated, param_vals = rr$learner$param_vals)
                   # }
                 )
)
