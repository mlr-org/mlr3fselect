#' @title Filter Base Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the base class for filters.
#' Predefined filters are stored in [mlr_filters].
#'
#' @section Construction:
#' ```
#' f = FilterResult$new(id, task_type, param_set, param_vals, feature_types, packages)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the filter.
#'
#' * `task_type` :: `character(1)`\cr
#'   Type of the task the filter can operator on. E.g., `"classif"` or `"regr"`.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `param_vals` :: named `list()`\cr
#'   Named list of hyperparameter settings.
#'
#' * `feature_types` :: `character()`\cr
#'   Feature types the filter operates on.
#'   Must be a subset of [`mlr_reflections$task_feature_types`][mlr3::mlr_reflections].
#'
#' * `task_properties` :: `character()`\cr
#'   Required task properties, see [mlr3::Task].
#'   Must be a subset of [`mlr_reflections$task_properties`][mlr3::mlr_reflections].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#'
#' @section Fields:
#'
#' * `id` :: `character(1)`\cr
#'   Stores the identifier of the filter.
#'
#' * `task_type` :: `character(1)`\cr
#'   Stores the type of class this filter can operate on, e.g. `"classif"` or `"regr"`.
#'   A complete list of task types is stored in [`mlr_reflections$task_types`][mlr3::mlr_reflections].
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Description of available hyperparameters and hyperparameter settings.
#'
#' * `feature_types` :: `character()`\cr
#'   Stores the feature types the filter can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
#'   A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr3::mlr_reflections].
#'
#' * `packages` :: `character()`\cr
#'   Stores the names of required packages.
#'
#' * `scores` :: `numeric()`\cr
#'   Stores the calculated filter score values as named numeric vector.
#'   The scores are sorted in decreasing order, with tied values in a random order.
#'
#' @section Methods:
#'
#' * `calculate(task)`\cr
#'   [Task] -> `numeric()`\cr
#'   Calculates the filter score values for the provided [Task] and stores them in field `scores`.
#'
#' * `filter_abs(task, abs)`\cr
#'   ([Task], `integer(1)`) -> [Task]\cr
#'   Filters the [Task] by reference, keeps up to `abs` features.
#'
#' * `filter_perc(task, perc)`\cr
#'   ([Task], `numeric(1)`) -> [Task]\cr
#'   Filters the [Task] by reference, keeps `perc` percent of the features (rounded via [base::round()]).
#'
#' * `filter_thresh(task, thresh)`\cr
#'   ([Task], `numeric(1)`) -> [Task]\cr
#'   Filters the [Task] by reference, keeps features whose filter score values exceeds `thresh`.
#'
#' @family Filter
#' @export
FilterResult = R6Class("FilterResult",
  public = list(
    id = NULL,
    task_type = NULL,
    task_properties = NULL,
    param_set = NULL,
    feature_types = NULL,
    packages = NULL,
    scores = NULL,

    initialize = function(id, task_type, task_properties = character(), param_set = ParamSet$new(), param_vals = list(), feature_types = character(), packages = character()) {
      self$id = assert_string(id)
      self$task_type = assert_subset(task_type, mlr_reflections$task_types, empty.ok = FALSE)
      self$task_properties = assert_subset(task_properties, unlist(mlr_reflections$task_properties, use.names = FALSE))
      self$param_set = assert_param_set(param_set)
      self$param_set$values = param_vals
      self$feature_types = assert_subset(feature_types, mlr_reflections$task_feature_types)
      self$packages = assert_character(packages, any.missing = FALSE, unique = TRUE)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      filter_print(self)
    },

    calculate = function(task) {

      assert_task(task, feature_types = self$feature_types, task_properties = self$task_properties)
      assert_filter_result(self, task)
      require_namespaces(self$packages)
      fn = task$feature_names

      fv = private$.calculate(task)
      assert_numeric(fv, len = length(fn), any.missing = FALSE)
      assert_names(names(fv), permutation.of = fn)

      self$scores = data.table(score = fv, feature = fn, method = self$id)[order(method, -score)]

      invisible(self)
    },

    combine = function(fr) {
      assert_filter_result(fr)
      self$scores = rbindlist(list(self$scores, fr$scores))
      invisible(self)
    },

    filter_abs = function(task, abs) {
      assert_task(task)
      assert_count(abs)
      filter_n(self, task, abs)
    },

    filter_perc = function(task, perc) {
      assert_task(task)
      assert_number(perc, lower = 0, upper = 1)
      filter_n(self, task, round(task$nrow * perc))
    },

    filter_thresh = function(task, threshold) {
      assert_task(task)
      assert_number(threshold)
      filter_n(self, task, sum(self$scores > threshold))
    }
  )
)

filter_n = function(self, task, n) {
  if (is.null(self$scores)) {
    stopf("Filter values have not been computed yet")
  }
  keep = names(head(self$scores, n))
  task$select(keep)
}

#' @export
as.data.table.FilterResult = function(x, ...) {
  fv = x$scores
  if (is.null(fv)) {
    stopf("No filter data available")
  }
  return(fv)
}

filter_print = function(self) {
  catf(format(self))
  catf(str_indent("Task Types:", self$task_type))
  catf(str_indent("Task Properties:", self$task_properties))
  catf(str_indent("Packages:", self$packages))
  catf(str_indent("Feature types:", self$feature_types))
}
