library(mlr3)
library(mlr3fselect)

# Random + Runtime
task = tsk("pima")
measure = msr("classif.acc")
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 10)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("random")
fs$select(instance)

instance$best()
instance$best_by_batch(2)

# Exhaustive + Runtime
task = tsk("pima")
measure = msr("classif.acc")
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 5)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("exhaustive")
fs$select(instance)

instance$best()
instance$best_by_batch(2)

# Exhaustive + Runtime
task = tsk("pima")
task$select(c("age", "insulin", "pregnant"))
measure = msr("classif.acc")
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 30)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("exhaustive")
fs$select(instance)

instance$best()
instance$best_by_batch(2)


# Sequential Forward + Runtime
task = tsk("pima")
measure = msr("classif.acc")
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = mlr3tuning::term("model_time", secs = 30)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("sequential")
fs$select(instance)

instance$best()
instance$best_by_batch(1)


# Sequential Backward + Runtime
task = tsk("pima")
measure = msr("classif.acc")
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 30)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("sequential")
fs$param_set$values = mlr3misc::insert_named(fs$param_set$values, list(strategy = "fsb"))
fs$select(instance)

instance$best()
instance$best_by_batch(1)


# Evolutionary + Runtime
task = tsk("pima")
measure = msr("classif.acc")
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("model_time", secs = 10)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("evolutionary")
fs$select(instance)

instance$best()
instance$best_by_batch(2)
