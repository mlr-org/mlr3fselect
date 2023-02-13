### SVM

set.seed(1)

at_1 = auto_fselector(
    method = fs("rfe", feature_number = 1, n_features = 1, aggregation = "mean"),
    learner = lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    callbacks = clbk("mlr3fselect.svm_rfe"),
    store_models = TRUE
)


at_2 = auto_fselector(
    method = fs("rfe", feature_number = 1, n_features = 1, aggregation = "rank"),
    learner = lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    callbacks = clbk("mlr3fselect.svm_rfe"),
    store_models = TRUE
)

at_3 = auto_fselector(
    method = fs("rfe", feature_number = 1, n_features = 1, aggregation = "rank"),
    learner = lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob"),
    resampling = rsmp("holdout"),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    callbacks = clbk("mlr3fselect.svm_rfe"),
    store_models = TRUE
)

at_4 = auto_fselector(
    method = fs("rfecv", feature_number = 1, n_features = 1),
    learner = lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    callbacks = clbk("mlr3fselect.svm_rfe"),
    store_models = TRUE
)

design = benchmark_grid(tsk("sonar"), list(at_1, at_2, at_3, at_4), rsmp("repeated_cv", folds = 5, repeats = 10))
bmr = benchmark(design)

bmr$aggregate(msr("classif.auc"))

#>    nr      resample_result task_id            learner_id resampling_id iters classif.auc
#> 1:  1 <ResampleResult[21]>   sonar classif.svm.fselector   repeated_cv    50   0.8061835
#> 2:  2 <ResampleResult[21]>   sonar classif.svm.fselector   repeated_cv    50   0.8137875
#> 3:  3 <ResampleResult[21]>   sonar classif.svm.fselector   repeated_cv    50   0.8165814
#> 4:  4 <ResampleResult[21]>   sonar classif.svm.fselector   repeated_cv    50   0.8092180

### Ranger

set.seed(1)

at_1 = auto_fselector(
    method = fs("rfe", feature_number = 1, n_features = 1, aggregation = "mean", recursive = FALSE),
    learner = lrn("classif.ranger", predict_type = "prob", importance = "permutation"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    store_models = TRUE
)


at_2 = auto_fselector(
    method = fs("rfe", feature_number = 1, n_features = 1, aggregation = "rank", recursive = FALSE),
    learner = lrn("classif.ranger", predict_type = "prob", importance = "permutation"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    store_models = TRUE
)

at_3 = auto_fselector(
    method = fs("rfe", feature_number = 1, n_features = 1, aggregation = "rank", recursive = FALSE),
    learner = lrn("classif.ranger", predict_type = "prob", importance = "permutation"),
    resampling = rsmp("holdout"),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    store_models = TRUE
)

at_4 = auto_fselector(
    method = fs("rfecv", feature_number = 1, n_features = 1, recursive = FALSE),
    learner = lrn("classif.ranger", predict_type = "prob", importance = "permutation"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.auc"),
    terminator = trm("none"),
    store_models = TRUE
)

design_2 = benchmark_grid(tsk("sonar"), list(at_1, at_2, at_3, at_4), rsmp("repeated_cv", folds = 5, repeats = 10))
bmr_2 = benchmark(design_2)

bmr_2$aggregate(msr("classif.auc"))

#>    nr      resample_result task_id               learner_id resampling_id iters classif.auc
#> 1:  1 <ResampleResult[21]>   sonar classif.ranger.fselector   repeated_cv    50   0.9160652
#> 2:  2 <ResampleResult[21]>   sonar classif.ranger.fselector   repeated_cv    50   0.9173909
#> 3:  3 <ResampleResult[21]>   sonar classif.ranger.fselector   repeated_cv    50   0.9236606
#> 4:  4 <ResampleResult[21]>   sonar classif.ranger.fselector   repeated_cv    50   0.9189643

rr = resample(tsk("sonar"), lrn("classif.ranger", predict_type = "prob"), rsmp("repeated_cv", folds = 5, repeats = 10))
rr$aggregate(msr("classif.auc"))

#> classif.auc
#>   0.9243887

 ### RFECV

library(mlr3learners)

instance = fsi(
    task = tsk("sonar"),
    learner = lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.auc"),
    terminator = trm("none"),
    callbacks = clbk("mlr3fselect.svm_rfe"),
    store_models = TRUE
)

optimizer = fs("rfecv", feature_number = 1, n_features = 1)

optimizer$optimize(instance)

instance$archive
