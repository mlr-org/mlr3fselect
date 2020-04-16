terminated_error = function(inst) {
  msg = sprintf(
    fmt = "FSelectInstance (tsk:%s, lrn:%s, term:%s) terminated",
    inst$objective$task$id,
    inst$objective$learner$id,
    format(inst$terminator)
  )

  set_class(list(message = msg, call = NULL),
    c("terminated_error", "error", "condition"))
}
