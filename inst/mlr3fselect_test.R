library(mlr3)
library(mlr3fselect)
library(mlr3learners)

# ref + evals
task = tsk("pima")
measure = msrs(c("classif.acc"))
learner = lrn("classif.rpart")
resampling = rsmp("cv")
terminator = term("evals", n_evals = 8)

instance = FSelectInstance$new(task, learner, resampling, measure, terminator)
fs = fs("rfe", recursive = TRUE)
fs$select(instance)



