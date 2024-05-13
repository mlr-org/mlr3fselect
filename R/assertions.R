#' @title Assertion for mlr3fselect objects
#'
#' @description
#' Most assertion functions ensure the right class attribute, and optionally additional properties.
#'
#' @name mlr3fselect_assertions
#' @keywords internal
NULL

assert_fselector = function(fselector) {
  assert_r6(fselector, "FSelector")
}

#' @export
#' @param fselectors (list of [FSelector]).
#' @rdname mlr3fselect_assertions
assert_fselectors = function(fselectors) {
  invisible(lapply(fselectors, assert_fselector))
}

#' @export
#' @param fselector (`FSelectorAsync`).
#' @rdname mlr3fselect_assertions
assert_fselector_async = function(fselector) {
  assert_r6(fselector, "FSelectorAsync")
}

#' @export
#' @param fselector ([FSelectorBatch]).
#' @rdname mlr3fselect_assertions
assert_fselector_batch = function(fselector) {
  assert_r6(fselector, "FSelectorBatch")
}

#' @export
#' @param inst ([FSelectInstanceBatchSingleCrit] | [FSelectInstanceBatchMultiCrit] | `FSelectInstanceAsyncSingleCrit` | `FSelectInstanceAsyncMultiCrit`).
#' @rdname mlr3fselect_assertions
assert_fselect_instance = function(inst) {
  assert_multi_class(inst, c(
    "FSelectInstanceBatchSingleCrit",
    "FSelectInstanceBatchMultiCrit",
    "FSelectInstanceAsyncSingleCrit",
    "FSelectInstanceAsyncMultiCrit"))
}

#' @export
#' @param inst (`FSelectInstanceAsyncSingleCrit` | `FSelectInstanceAsyncMultiCrit`).
#' @rdname mlr3fselect_assertions
assert_fselect_instance_async = function(inst) {
  assert_multi_class(inst, c(
    "FSelectInstanceAsyncSingleCrit",
    "FSelectInstanceAsyncMultiCrit"))
}

#' @export
#' @param inst ([FSelectInstanceBatchSingleCrit] | [FSelectInstanceBatchMultiCrit]).
#' @rdname mlr3fselect_assertions
assert_fselect_instance_batch = function(inst) {
  assert_multi_class(inst, c(
    "FSelectInstanceBatchSingleCrit",
    "FSelectInstanceBatchMultiCrit"))
}

