#' @title Class for Feature Selection Algorithms
#'
#' @include mlr_fselectors.R
#'
#' @description
#' The [FSelector] implements the optimization algorithm.
#'
#' @details
#' [FSelector] is a abstract base class that implements the base functionality each fselector must provide.
#' A subclass is implemented in the following way:
#'  * Inherit from FSelector.
#'  * Specify the private abstract method `$.optimize()` and use it to call into your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [FSelectInstanceSingleCrit]/[FSelectInstanceMultiCrit] object `instance`, so each batch is possibly executed in parallel via [mlr3::benchmark()], and all evaluations are stored inside of `instance$archive`.
#'  * Before the batch evaluation, the [bbotk::Terminator] is checked, and if it is positive, an exception of class `"terminated_error"` is generated.
#'    In the  later case the current batch of evaluations is still stored in `instance`, but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best set from `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [bbotk::Terminator] may be evaluated, as the Terminator is only checked before a batch evaluation, and not in-between evaluation in a batch.
#'    How many more depends on the setting of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide yourself how to estimate the final set in the instance and its estimated performance.
#'    The default behavior is: We pick the best resample-experiment, regarding the given measure, then assign its set and aggregated performance to the instance.
#'
#' @section Private Methods:
#' * `.optimize(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify feature selection of your subclass.
#'   See technical details sections.
#' * `.assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final feature subset is  selected.
#'   See technical details sections.
#'
#' @section Resources:
#' * [book section](https://mlr3book.mlr-org.com/feature-selection.html#the-fselector-class) on feature selection algorithms.
#'
#' @template param_man
#'
#' @export
FSelector = R6Class("FSelector",
  public = list(

    #' @field id (`character(1)`)\cr
    #'   Identifier of the object.
    #'   Used in tables, plot and text output.
    id = NULL,

    #' @description
    #'   Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the new instance.
    #'
    #' @param param_set [paradox::ParamSet]\cr
    #'   Set of control parameters.
    #'
    #' @param properties (`character()`)\cr
    #'   Set of properties of the fselector.
    #'   Must be a subset of [`mlr_reflections$fselect_properties`][mlr3::mlr_reflections].
    #'
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
    #'
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #'   Can be used in tables, plot and text output instead of the ID.
    initialize = function(id = "fselector", param_set, properties, packages = character(), label = NA_character_, man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      private$.param_set = assert_param_set(param_set)
      private$.properties = assert_subset(properties, bbotk_reflections$optimizer_properties, empty.ok = FALSE)
      private$.packages = union("mlr3fselect", assert_character(packages, any.missing = FALSE, min.chars = 1L))
      private$.label = assert_string(label, na.ok = TRUE)
      private$.man = assert_string(man, na.ok = TRUE)

      check_packages_installed(self$packages, msg = sprintf("Package '%%s' required but not installed for FSelector '%s'", format(self)))
    },

    #' @description
    #' Helper for print outputs.
    #'
    #' @return (`character()`).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), if (is.na(self$label)) "" else paste0(": ", self$label))
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catf(str_indent("* Properties:", self$properties))
      catf(str_indent("* Packages:", self$packages))
    },


    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstanceSingleCrit] or [FSelectInstanceMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveFSelect] that resides in the [FSelectInstanceSingleCrit] / [FSelectInstanceMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([FSelectInstanceSingleCrit] | [FSelectInstanceMultiCrit]).
    #'
    #' @return [data.table::data.table()].
    optimize = function(inst) {
      assert_multi_class(inst, c("FSelectInstanceSingleCrit", "FSelectInstanceMultiCrit"))
      inst$.__enclos_env__$private$.context = ContextOptimization$new(instance = inst, optimizer = self)
      call_back("on_optimization_begin", inst$callbacks, get_private(inst)$.context)
      result = optimize_default(inst, self, private)
      call_back("on_optimization_end", inst$callbacks, get_private(inst)$.context)
      result
    }
  ),

  active = list(

    #' @field param_set [paradox::ParamSet]\cr
    #' Set of control parameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$param_set is read-only.")
      }
      private$.param_set
    },

    #' @field properties (`character()`)\cr
    #' Set of properties of the fselector.
    #' Must be a subset of [`mlr_reflections$fselect_properties`][mlr3::mlr_reflections].
    properties = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.properties)) {
        stop("$properties is read-only.")
      }
      private$.properties
    },

    #' @field packages (`character()`)\cr
    #' Set of required packages.
    #' Note that these packages will be loaded via [requireNamespace()], and are not attached.
    packages = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.packages)) {
        stop("$packages is read-only.")
      }
      private$.packages
    },

    #' @field label (`character(1)`)\cr
    #' Label for this object.
    #' Can be used in tables, plot and text output instead of the ID.
    label = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$label is read-only.")
      }
      private$.label
    },

    #' @field man (`character(1)`)\cr
    #' String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    #' The referenced help package can be opened via method `$help()`.
    man = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.man)) {
        stop("$man is read-only.")
      }
      private$.man
    }
  ),

  private = list(
    .optimize = function(inst) stop("abstract"),

    .assign_result = function(inst) {
      assert_multi_class(inst, c("FSelectInstanceSingleCrit", "FSelectInstanceMultiCrit"))
      assign_result_default(inst)
    },

    .param_set = NULL,
    .param_classes = "ParamLgl", # keeps compatibility to bbotk
    .properties = NULL,
    .packages = NULL,
    .label = NULL,
    .man = NULL
  )
)
