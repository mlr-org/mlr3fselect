library(mlr3fswrap)

# Random + Runtime

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 10, units = "secs")
fs = FeatureSelectionRandom$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Random + Runtime + Maximum Features

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 10, units = "secs")
fs = FeatureSelectionRandom$new(pe, tm, measure, param_vals = list(max_features = 2))
fs$calculate()

fs$get_result()

# Random + Performance Maximize

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorPerformance$new(threshold = 0.75)
fs = FeatureSelectionRandom$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Random + Performance Minimize

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.ce"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorPerformance$new(threshold = 0.25)
fs = FeatureSelectionRandom$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Random + Performance Step

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorPerformanceStep$new(threshold = 0.01)
fs = FeatureSelectionRandom$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Random + Evaluations

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorEvaluations$new(max_evaluations = 20)
fs = FeatureSelectionRandom$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Exhaustive + Runtime

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 5, units = "secs")
fs = FeatureSelectionExhaustive$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Exhaustive + Performance Step

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorPerformanceStep$new(threshold = 0.01)
fs = FeatureSelectionExhaustive$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Exhaustive + Evaluations

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorEvaluations$new(max_evaluations = 20)
fs = FeatureSelectionExhaustive$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Exhaustive + Evaluations + Max Features

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorEvaluations$new(max_evaluations = 10000)
fs = FeatureSelectionExhaustive$new(pe, tm, measure, param_vals = list(max_features = 2))
fs$calculate()

fs$get_result()

# Sequential Forward + Runtime

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 30, units = "secs")
fs = FeatureSelectionSequential$new(pe, tm, measure)
fs$calculate()

fs$get_result()
fs$get_path()

# Sequential Forward + Runtime + Maximum Features

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 10, units = "secs")
fs = FeatureSelectionSequential$new(pe, tm, measure, param_vals = list(max_features = 2))
fs$calculate()

fs$get_result()

# Sequential Forward + Performance Step

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorPerformanceStep$new(threshold = 0.001)
fs = FeatureSelectionSequential$new(pe, tm, measure)
fs$calculate()

fs$get_result()

# Sequential Forward + Evaluations

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorEvaluations$new(max_evaluations = 20)
fs = FeatureSelectionSequential$new(pe, tm, measure)
fs$calculate()

fs$get_result()


# Sequential Backward + Runtime

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 30, units = "secs")
fs = FeatureSelectionSequential$new(pe, tm, measure, param_vals = list(strategy = "fsb"))
fs$calculate()

fs$get_result()
fs$get_path()

# Sequential Backward + Runtime + Maximum Features
# Max features is ignored

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 10, units = "secs")
fs = FeatureSelectionSequential$new(pe, tm, measure, param_vals = list(max_features = 4,
                                                                       strategy = "fsb"))
fs$calculate()

fs$get_result()

# Sequential Backward + Performance Step

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorPerformanceStep$new(threshold = 0.001)
fs = FeatureSelectionSequential$new(pe, tm, measure, param_vals = list(strategy = "fsb"))
fs$calculate()

fs$get_result()

# Sequential Backward + Evaluations

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorEvaluations$new(max_evaluations = 20)
fs = FeatureSelectionSequential$new(pe, tm, measure, param_vals = list(strategy = "fsb"))
fs$calculate()

fs$get_result()


# Genetic + Runtime + Plus

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 5, units = "secs")
fs = FeatureSelectionGenetic$new(pe, tm, measure, param_vals = list(strategy = "plus",
                                                                    lambda = 5,
                                                                    mu = 10))
fs$calculate()

fs$get_result()
fs$get_path()


# Genetic + Runtime + Comma

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 5, units = "secs")
fs = FeatureSelectionGenetic$new(pe, tm, measure, param_vals = list(strategy = "comma",
                                                                    lambda = 20,
                                                                    mu = 10))
fs$calculate()

fs$get_result()
fs$get_path()

# Genetic + Runtime + Comma + Max Feataures

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 5, units = "secs")
fs = FeatureSelectionGenetic$new(pe, tm, measure, param_vals = list(strategy = "comma",
                                                                    lambda = 20,
                                                                    mu = 10,
                                                                    max_features = 4))
fs$calculate()

fs$get_result()
fs$get_path()

# Genetic + Runtime + Comma + Mutation Rate

task = mlr3::mlr_tasks$get("pima")
measure = mlr3::mlr_measures$get(c("classif.acc"))
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5))
resampling$instantiate(task)
pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
tm = TerminatorRuntime$new(max_time = 5, units = "secs")
fs = FeatureSelectionGenetic$new(pe, tm, measure, param_vals = list(strategy = "comma",
                                                                    lambda = 20,
                                                                    mu = 10,
                                                                    mutation_rate = 0))
fs$calculate()

fs$get_result()
fs$get_path()
