test_that("ArchiveFSelect access methods work", {
  instance = fselect(
    method = "random_search",
    task = tsk("iris"),
    learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
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
  map(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })
})

test_that("ArchiveTuning as.data.table function works", {
  instance = fselect(
    method = "random_search",
    task = tsk("pima"),
    learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    term_evals = 4,
    batch_size = 4)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 13)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'runtime', 'timestamp', 'batch_nr', 'resample_result'))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, nrows = 4, ncols = 14)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'classif.acc', 'runtime', 'timestamp', 'batch_nr', 'resample_result'))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, nrows = 4, ncols = 15)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'classif.acc', 'classif.mcc', 'runtime', 'timestamp', 'batch_nr', 'resample_result'))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp")
  expect_data_table(tab, nrows = 4, ncols = 13)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'runtime', 'batch_nr', 'uhash', 'resample_result'))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp", "uhash"))
  expect_data_table(tab, nrows = 4, ncols = 12)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'runtime', 'batch_nr', 'resample_result'))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, nrows = 4, ncols = 14)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'runtime', 'timestamp', 'batch_nr', 'uhash', 'resample_result'))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, nrows = 4, ncols = 13)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'runtime', 'timestamp', 'batch_nr', 'resample_result'))

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
  expect_data_table(tab, nrows = 4, ncols = 12)
  expect_named(tab, c('age', 'glucose', 'insulin', 'mass', 'pedigree', 'pregnant', 'pressure', 'triceps', 'classif.ce', 'runtime', 'timestamp', 'batch_nr'))

  # empty archive
  instance = FSelectInstanceSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4))

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)
})
