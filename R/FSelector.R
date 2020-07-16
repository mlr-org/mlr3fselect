#' @title FSelector
#'
#' @description
#' Abstract `FSelector` class that implements the base functionality each
#' `FSelector` must provide. A `FSelector` object describes the feature selection
#' strategy, i.e. how to optimize the black-box function and its feasible set
#' defined by the [FSelectInstanceSingleCrit] / [FSelectInstanceMultiCrit] object.
#'
#' A tuner must write its result into the [FSelectInstanceSingleCrit] /
#' [FSelectInstanceMultiCrit] using the `assign_result` method of the
#' [bbotk::OptimInstance] at the end of its selection in order to store the best
#' selected feature subset and its estimated performance vector.
#'
#' @section Private Methods:
#' * `.optimize(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify feature selection of your
#'   subclass. See technical details sections.
#' * `.assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final feature subset is
#'   selected. See technical details sections.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from `FSelector`.
#'  * Specify the private abstract method `$.optimize()` and use it to call into
#'    your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate feature subsets.
#'  * The batch evaluation is requested at the [FSelectInstanceSingleCrit] /
#'    [FSelectInstanceMultiCrit] object `instance`, so each batch is possibly
#'    executed in parallel via [mlr3::benchmark()], and all evaluations are stored
#'    inside of `instance$archive`.
#'  * Before the batch evaluation, the [Terminator] is checked, and if it is
#'    positive, an exception of class `"terminated_error"` is generated. In the
#'    later case the current batch of evaluations is still stored in `instance`,
#'    but the numeric scores are not sent back to the handling optimizer as it has
#'    lost execution control.
#'  * After such an exception was caught we select the best feature subset from
#'    `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [Terminator] may be
#'    evaluated, as the Terminator is only checked before a batch evaluation, and
#'    not in-between evaluation in a batch. How many more depends on the setting
#'    of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide
#'    yourself how to estimate the final feature subset in the instance and its
#'    estimated performance. The default behavior is: We pick the best
#'    resample-experiment, regarding the given measure, then assign its
#'    feature subset and aggregated performance to the instance.
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' terminator = trm("evals", n_evals = 3)
#'
#' instance = FSelectInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' # swap this line to use a different FSelector
#' fs = fs("random_search")
#'
#' # modifies the instance by reference
#' fs$optimize(instance)
#'
#' # returns best feature subset and best performance
#' instance$result
#'
#' # allows access of data.table / benchmark result of full path of all
#' # evaluations
#' instance$archive
FSelector = R6Class("FSelector",
  public = list(

    param_set = NULL,
    properties = NULL,
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set [paradox::ParamSet]\cr
    #' Set of control parameters for fselector.
    #'
    #' @param properties (`character()`)\cr
    #' Set of properties of the fselector. Must be a subset of
    #' [`mlr_reflections$fselect_properties`][mlr3::mlr_reflections].
    #'
    #' @param packages (`character()`)\cr
    #' Set of required packages. Note that these packages will be loaded via
    #' [requireNamespace()], and are not attached.
    initialize = function(param_set, properties, packages = character(0)) {
      self$param_set = assert_param_set(param_set)
      self$properties = assert_subset(properties,
        bbotk_reflections$optimizer_properties,
        empty.ok = FALSE)
      self$packages = assert_set(packages)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstanceSingleCrit] or
    #' [FSelectInstanceMultiCrit] until termination.The single evaluations and
    #' the final results will be written into the [bbotk::Archive] that resides
    #' in the [FSelectInstanceSingleCrit] / [FSelectInstanceMultiCrit].
    #'
    #' @param inst ([FSelectInstanceSingleCrit]|[FSelectInstanceMultiCrit]).
    optimize = function(inst) {
      assert_multi_class(inst, c("FSelectInstanceSingleCrit", "FSelectInstanceMultiCrit"))
      optimize_default(inst, self, private)
    }
  ),

  private = list(
    .optimize = function(inst) stop("abstract"),

    .assign_result = function(inst) {
      assert_multi_class(inst, c("FSelectInstanceSingleCrit", "FSelectInstanceMultiCrit"))
      assign_result_default(inst)
    }
  )
)
