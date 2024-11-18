test_that("extract_inner_fselect_archives function works with resample and cv", {
  rr = fselect_nested(fs("random_search", batch_size = 1), tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), rsmp("cv", folds = 2),
    msr("classif.ce"), term_evals = 4)

  irr = extract_inner_fselect_archives(rr)
  expect_data_table(irr, nrows = 8)
  expect_names(names(irr), must.include = c("iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "n_features", "resample_result", "task_id", "learner_id", "resampling_id"))
})

test_that("extract_inner_fselect_archives function works with resample and repeated cv", {
  rr = fselect_nested(fs("random_search", batch_size = 1), tsk("iris"), lrn("classif.rpart"), rsmp("holdout"),
    rsmp("repeated_cv", folds = 2, repeats = 3), msr("classif.ce"), term_evals = 4)

  irr = extract_inner_fselect_archives(rr)
  expect_data_table(irr, nrows = 24)
  expect_names(names(irr), must.include = c("iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "n_features", "resample_result", "task_id", "learner_id", "resampling_id"))
})

test_that("extract_inner_fselect_archives function works with benchmark and cv", {
  at_1 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_1")
  at_2 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_2")
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_archives(bmr)
  expect_data_table(ibmr, nrows = 16)
  expect_names(names(ibmr), must.include = c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "n_features", "resample_result", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), c(1, 2))
})

test_that("extract_inner_fselect_archives function works with benchmark and repeated cv", {
  at_1 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_1")
  at_2 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_2")
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_archives(bmr)
  expect_data_table(ibmr, nrows = 48)
  expect_names(names(ibmr), must.include = c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "n_features", "resample_result", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), c(1, 2))
})

test_that("extract_inner_fselect_archives function works with multiple tasks", {
  at_1 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_1")
  at_2 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_2")
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(list(tsk("iris"), tsk("pima")), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_archives(bmr)
  expect_data_table(ibmr, nrows = 32)
  expect_names(names(ibmr), must.include = c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "n_features", "resample_result", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), c(1, 2, 3, 4))
})

test_that("extract_inner_fselect_archives function works with no models", {
  at = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(tsk("iris"), at, resampling_outer, store_models = FALSE)

  expect_data_table(extract_inner_fselect_archives(rr), nrows = 0, ncols = 0)
})

test_that("extract_inner_fselect_archives function works with no instance", {
  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 4),
  fselector = fs("random_search", batch_size = 1), store_fselect_instance = FALSE, store_benchmark_result = FALSE)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)

  expect_data_table(extract_inner_fselect_archives(rr), nrows = 0, ncols = 0)
})

test_that("extract_inner_fselect_archives function works with benchmark and no models", {
  at_1 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_1")
  at_2 = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4, id = "at_2")
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = FALSE)

  expect_data_table(extract_inner_fselect_archives(bmr), nrows = 0, ncols = 0)
})

test_that("extract_inner_fselect_archives function works with mixed store instance", {
  at_1 = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 4),
  fselector = fs("random_search", batch_size = 1), store_fselect_instance = FALSE, store_benchmark_result = FALSE, id = "at_1")
  at_2 = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 4),
  fselector = fs("random_search", batch_size = 1), id = "at_2")
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_archives(bmr)
  expect_data_table(ibmr)
  expect_equal(unique(ibmr$experiment), 2)
})

test_that("extract_inner_fselect_archives function works with autofselector and learner", {
  learner = lrn("classif.rpart")
  at = auto_fselector(fs("random_search", batch_size = 1), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(tsk("iris"), list(at, learner), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_archives(bmr)
  expect_data_table(ibmr, nrows = 8)
  expect_names(names(ibmr), must.include = c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "n_features", "resample_result", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), 1)
})
