terminated_error = function(instance) {
  msg = sprintf(
    fmt = "FSelectInstance (tsk:%s, lrn:%s, term:%s) terminated",
    instance$task$id,
    instance$learner$id,
    format(instance$terminator)
  )

  set_class(list(message = msg, call = NULL), c("terminated_error", "error", "condition"))
}
