#' @title FSelector
#'
#' @include mlr_fselectors.R
#'
#' @description
#' The `FSelector`` implements the optimization algorithm.
#'
#' @details
#' `FSelector` is an abstract base class that implements the base functionality each fselector must provide.
#'
#' @section Resources:
#' There are several sections about feature selection in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' * Learn more about [fselectors](https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#the-fselector-class).
#'
#' The [gallery](https://mlr-org.com/gallery.html) features a collection of case studies and demos about optimization.
#'
#' * Utilize the built-in feature importance of models with [Recursive Feature Elimination](https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/).
#' * Run a feature selection with [Shadow Variable Search](https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/).
#'
#' @family FSelector
#' @template field_id
#'
#' @template param_id
#' @template param_param_set
#' @template param_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @export
FSelector = R6Class("FSelector",
  public = list(

    id = NULL,

    #' @description
    #'   Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "fselector",
      param_set,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
      ) {
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
    #' @param ... (ignored).
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
      assert_fselect_instance(inst)
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
