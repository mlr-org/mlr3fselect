#' @title Linear Correlation
#'
#' @description
#' Filter.stats_linear.correlation
#'
#' @section Usage:
#' ```
#' filter = Filter.stats_linear.correlation$new(task, settings)
#' ```
#' See [Filter] for a description of the interface.
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#' @section Details:
#' `$new()` creates a new object of class [Filter.stats_linear.correlation].
#' The interface is described in [Filter].
#'
#' @name Filter.stats_linear.correlation
#' @keywords internal
#' @family Filter
#' @examples
#' task_iris = mlr3::mlr_tasks$get("iris")
#' filter = Filter.stats_linear.correlation$new(task = task_iris)
#' filter$createFilterValuesData(task_iris)
NULL

#' @export
#' @include Filter.R
Filter.stats_linear.correlation = R6Class("Filter.stats_linear.correlation",
                          inherit = Filter,
                          public = list(
                            task = NULL,
                            initialize = function(id, settings = list(), packages,
                                                  task) {
                              super$initialize(
                                id = "Filter.stats_linear.correlation",
                                settings = settings,
                                packages = "stats",
                                task = task
                              )
                          }),

                            #   if (is.null(resolution)) {
                            #     remaining = terminator$remaining
                            #     assert_int(remaining, lower = 1L)
                            #     resolution = floor(remaining / ff$param_set$length)
                            #   }
                            #   resolution = assert_int(resolution, lower = 1L, coerce = TRUE)
                            #   super$initialize(id = "grid_search", ff = ff, terminator = terminator, settings = list(resolution = resolution))
                          #  }
                          #),

                          private = list(
                            createFilterValuesData_step = function(task) {

                              browser()
                              # mlr implementation
                              # abs(cor(as.matrix(data$data), data$target, use = "pairwise.complete.obs", method = "pearson")[, 1L])

                              invoke(
                                stats::cor(),
                                x = as.matrix(task$data()),
                                y = as.matrix(task$data(cols = task$target_names)), # simpler way to subset target?
                                use = "pairwise.complete.obs",
                                method = "pearson"
                              )

                              print("test")
                            }
                          )
)

