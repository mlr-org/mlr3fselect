library(mlr3)
library(mlr3fselect)

# random + evals
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("evals", n_evals = 8)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("random")
fs$select(instance)

# random + clock_time
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("clock_time", secs = 5)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("random")
fs$select(instance)

# random + model_time
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 5)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("random")
fs$select(instance)

# random + perf_reached
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("perf_reached", level = 0.75)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("random")
fs$select(instance)

# sequential + stagnation

task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("stagnation", iters = 10, threshold = 0.01)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("sequential")
fs$select(instance)

# sequential + model_time

task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 30)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("sequential")
fs$select(instance)

instance$optimization_path(n = 1, m = 1:8)
instance$result

# sequential backward + model_time

task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 30)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("sequential", strategy = "fsb")
fs$select(instance)

instance$optimization_path(n = 1, m = 1:8)
instance$result

# evolutionary +  model_time

task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 30)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("evolutionary", mu = 25 , lambda = 30)
fs$select(instance)

instance$result

# ref + evals
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("evals", n_evals = 8)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("rfe", recursive = TRUE)
fs$select(instance)

instance$result

# ref + evals
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("evals", n_evals = 8)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("rfe")
fs$select(instance)

instance$result
