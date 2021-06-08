test_that("extract_inner_fselect_results function works", {
  # cv
  rr = fselect_nested("random_search", tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), rsmp("cv", folds = 2),
    msr("classif.ce"), term_evals = 4)

  irr = extract_inner_fselect_results(rr)
  expect_data_table(irr, nrows = 2)
  expect_named(irr, c("iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", 
    "features", "task_id", "learner_id", "resampling_id"))

  # repeated cv
  rr = fselect_nested("random_search", tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), 
    rsmp("repeated_cv", folds = 2, repeats = 3), msr("classif.ce"), term_evals = 4)
 
  irr = extract_inner_fselect_results(rr)
  expect_data_table(irr, nrows = 6)
  expect_named(irr, c("iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "classif.ce", 
    "features", "task_id", "learner_id", "resampling_id"))

  # cv
  at_1 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  at_2 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_results(bmr)
  expect_data_table(ibmr, nrows = 4)
  expect_named(ibmr, c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", 
    "classif.ce", "features", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

   # repeated cv
  at_1 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  at_2 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_results(bmr)
  expect_data_table(ibmr, nrows = 12)
  expect_named(ibmr, c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", 
    "classif.ce", "features", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # different tasks
  at_1 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  at_2 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(list(tsk("iris"), tsk("pima")), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_results(bmr)
  expect_data_table(ibmr, nrows = 8)
  expect_named(ibmr, c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "age", 
    "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce", "features", "task_id", 
    "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), c(1, 2, 3, 4))

  # no model
  at = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(tsk("iris"), at, resampling_outer, store_models = FALSE)

  expect_data_table(extract_inner_fselect_results(rr), nrows = 0, ncols = 0)

 # instance
  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 4), 
    fselector = fs("random_search"), store_fselect_instance = FALSE, store_benchmark_result = FALSE)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)

  expect_data_table(extract_inner_fselect_results(rr), nrows = 0, ncols = 0)

  # no models
  at_1 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  at_2 = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = FALSE)

  expect_data_table(extract_inner_fselect_results(bmr), nrows = 0, ncols = 0)

  # https://github.com/mlr-org/mlr3/issues/647
  # mixed store instance
  # at_1 = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 4), 
  #   fselector = fs("random_search"), store_fselect_instance = FALSE, store_benchmark_result = FALSE)
  # at_2 = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), trm("evals", n_evals = 4), 
  #   fselector = fs("random_search"))
  # resampling_outer = rsmp("cv", folds = 2)
  # grid = benchmark_grid(tsk("iris"), list(at_1, at_2), resampling_outer)
  # bmr = benchmark(grid, store_models = TRUE)

  # ibmr = extract_inner_fselect_results(bmr)
  # expect_data_table(ibmr, nrows = 2, ncols = 9)
  # expect_equal(unique(ibmr$experiment), 2)

  # autotuner and learner
  learner = lrn("classif.rpart")
  at = auto_fselector("random_search", lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term_evals = 4)
  grid = benchmark_grid(tsk("iris"), list(at, learner), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_fselect_results(bmr)
  expect_data_table(ibmr, nrows = 2)
  expect_named(ibmr, c("experiment", "iteration", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", 
    "classif.ce", "features", "task_id", "learner_id", "resampling_id"))
  expect_equal(unique(ibmr$experiment), 1)
})
