#' @title Gain Ratio
#'
#' @description
#' FilterGain Ratio
#'
#' @section Usage:
#' ```
#' filter = FilterGainRatio$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterGainRatio].
#'
#' @name FilterGainRatio
#' @family Filter
#' @examples
#' task = mlr_tasks$get("bh")
#' filter = FilterGainRatio$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterGainRatio = R6Class("FilterGainRatio",
                          inherit = Filter,
                          public = list(
                            initialize = function(id, packages,
                                                  feature_types,
                                                  task_type,
                                                  settings = list()) {
                              super$initialize(
                                id = "FilterGainRatio",
                                packages = "FSelectorRcpp",
                                feature_types = c("numeric", "integer", "factor", "ordered"),
                                task_type = c("classif", "regr"),
                                settings = settings)
                            },
                            calculate = function(task, settings = self$settings) {

                              # check for supported features
                              assert_feature_types(task, self)
                              # check for supported task
                              assert_filter(filter, task)

                              # check for Namespace
                              require_namespaces(self$packages)

                              # assign task to class
                              self$task = task

                              browser()

                              x = as.data.frame(task$data(cols = task$feature_names))
                              y = task$data(cols = task$target_names)[[task$target_names]]

                              filter_values = invoke(FSelectorRcpp::information_gain,
                                x = x, y = y, type = "gainratio", .args = settings)
                              filter_values = setNames(filter_values$importance,
                                filter_values$attributes)
                              self$filterValues = filter_values
                            }
                          )
)

