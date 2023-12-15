test_that("ArchiveFSelect access methods work", {
  instance = fselect(
    fselector = fs("random_search"),
    task = tsk("iris"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    term_evals = 4)

  # learner
  map(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  # learners
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learners(uhash = uhash))
    expect_learner(instance$archive$learners(uhash = uhash)[[1]])
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$predictions(uhash = uhash))
    expect_prediction(instance$archive$predictions(uhash = uhash)[[1]])
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result

# Issue https://github.com/mlr-org/mlr3/issues/893
#  map(instance$archive$data$uhash, function(uhash) {
#    expect_resample_result(instance$archive$resample_result(uhash = uhash))
#  })
#
#  map(seq(nrow(instance$archive$data)), function(i) {
#    expect_resample_result(instance$archive$resample_result(i))
#  })
})

test_that("ArchiveFSelect as.data.table function works", {
  instance = fselect(
    fselector = fs("random_search", batch_size = 4),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    term_evals = 4)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 16)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "resample_result"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, nrows = 4, ncols = 17)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "classif.acc", "runtime_learners", "timestamp", "batch_nr",  "warnings", "errors", "features", "resample_result"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, nrows = 4, ncols = 18)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "classif.acc", "classif.mcc", "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features", "resample_result"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp")
  expect_data_table(tab, nrows = 4, ncols = 16)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "runtime_learners", "batch_nr", "uhash", "warnings", "errors", "features", "resample_result"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp", "uhash"))
  expect_data_table(tab, nrows = 4, ncols = 15)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "runtime_learners", "batch_nr",  "warnings", "errors", "features", "resample_result"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, nrows = 4, ncols = 17)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "runtime_learners", "timestamp", "batch_nr",  "uhash", "warnings", "errors", "features", "resample_result"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, nrows = 4, ncols = 16)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "runtime_learners", "timestamp", "batch_nr",  "warnings", "errors", "features", "resample_result"))

  # without benchmark result
  instance = FSelectInstanceSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    store_benchmark_result = FALSE)
  fselector = fs("random_search", batch_size = 4)
  fselector$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 15)
  expect_named(tab, c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce",
    "runtime_learners", "timestamp", "batch_nr", "warnings", "errors", "features"))

  # empty archive
  instance = FSelectInstanceSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4))

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)

  # row order
  instance = fselect(
    fselector = fs("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    term_evals = 10)

  tab = as.data.table(instance$archive)
  expect_equal(tab$batch_nr, 1:10)
})

test_that("best method works with ties", {

  design = mlr3misc::rowwise_table(
    ~x1,   ~x2,   ~x3,    ~x4,
    TRUE,  FALSE, FALSE,  TRUE,
    TRUE,  FALSE, FALSE,  FALSE,
    TRUE,  TRUE,  FALSE,  FALSE
  )

  score_design = data.table(
    score = c(0.2, 0.2, 0.1),
    features = list(c("x1", "x4"), "x1", c("x1", "x2"))
  )
  measure = msr("dummy", score_design = score_design, minimize = FALSE)

  instance = fselect(
    fselector = fs("design_points", design = design),
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = measure
  )

  instance$archive$best()
  instance$archive$best(ties_method = "first")
  instance$archive$best(ties_method = "random")
  instance$archive$best(ties_method = "n_features")
})

test_that("best method works with ties and maximization", {

  design = mlr3misc::rowwise_table(
    ~x1,   ~x2,   ~x3,    ~x4,
    TRUE,  FALSE, FALSE,  TRUE,
    TRUE,  FALSE, FALSE,  FALSE,
    FALSE, TRUE,  FALSE,  FALSE
  )

  score_design = data.table(
    score = c(0.2, 0.2, 0.1),
    features = list(c("x1", "x4"), "x1", c("x1", "x2"))
  )
  measure = msr("dummy", score_design = score_design, minimize = FALSE)

  instance = fselect(
    fselector = fs("design_points", design = design),
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = measure
  )

  expect_features(instance$archive$best()[, list(x1, x2, x3, x4)], identical_to = c("x1", "x4"))
  expect_features(instance$archive$best(ties_method = "first")[, list(x1, x2, x3, x4)], identical_to = c("x1", "x4"))
  expect_features(instance$archive$best(ties_method = "random")[, list(x1, x2, x3, x4)], must_include = "x1")
  expect_features(instance$archive$best(ties_method = "n_features")[, list(x1, x2, x3, x4)], identical_to = "x1")
})

test_that("best method works with ties and minimization", {

  design = mlr3misc::rowwise_table(
    ~x1,   ~x2,   ~x3,    ~x4,
    TRUE,  FALSE, FALSE,  TRUE,
    TRUE,  FALSE, FALSE,  FALSE,
    FALSE, TRUE,  FALSE,  FALSE,
    FALSE, TRUE,  FALSE,  TRUE
  )

  score_design = data.table(
    score = c(0.2, 0.2, 0.1, 0.1),
    features = list(c("x1", "x4"), "x1", c("x1", "x2"), c("x2", "x4"))
  )
  measure = msr("dummy", score_design = score_design, minimize = TRUE)

  instance = fselect(
    fselector = fs("design_points", design = design),
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = measure
  )

  expect_features(instance$archive$best()[, list(x1, x2, x3, x4)], identical_to = "x2")
  expect_features(instance$archive$best(ties_method = "first")[, list(x1, x2, x3, x4)], identical_to = "x2")
  expect_features(instance$archive$best(ties_method = "random")[, list(x1, x2, x3, x4)], must_include = "x2")
  expect_features(instance$archive$best(ties_method = "n_features")[, list(x1, x2, x3, x4)], identical_to = "x2")
})

test_that("best method works with batches and ties", {

  design = mlr3misc::rowwise_table(
    ~x1,   ~x2,   ~x3,    ~x4,
    TRUE,  FALSE, FALSE,  TRUE,
    TRUE,  FALSE, FALSE,  FALSE,
    FALSE, TRUE,  TRUE,   FALSE,
    FALSE, TRUE,  FALSE,  FALSE
  )

  score_design = data.table(
    score = c(0.2, 0.2, 0.2, 0.1),
    features = list(c("x1", "x4"), "x1", c("x2", "x3"), c("x1", "x2"))
  )
  measure = msr("dummy", score_design = score_design, minimize = FALSE)

  instance = fselect(
    fselector = fs("design_points", design = design, batch_size = 1),
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = measure
  )

  expect_features(instance$archive$best(batch = c(2, 3))[, list(x1, x2, x3, x4)], identical_to = "x1")
  expect_features(instance$archive$best(batch = c(1, 3), ties_method = "first")[, list(x1, x2, x3, x4)], identical_to = c("x1", "x4"))
  expect_features(instance$archive$best(batch = c(1, 2), ties_method = "random")[, list(x1, x2, x3, x4)], must_include = "x1")
  expect_features(instance$archive$best(batch = c(2, 3), ties_method = "n_features")[, list(x1, x2, x3, x4)], identical_to = "x1")
})
