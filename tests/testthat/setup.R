library(mlr3)

loggers = list(lgr::get_logger("mlr3"), lgr::get_logger("bbotk"))
thresholds = mlr3misc::map_int(loggers, "threshold")
lapply(loggers, function(l) l$set_threshold("warn"))

set.seed(123)
