#' @export
ensemble_fselect = function(task, learners, outer_resampling, inner_resampling, fselector, terminator) {
  assert_task(task)
  assert_learners(learners)
  assert_resampling(outer_resampling)
  assert_resampling(inner_resampling)
  assert_fselector(fselector)
  assert_terminator(terminator)

  outer_resampling$instantiate(task)

  grid = map_dtr(seq(outer_resampling$iters), function(i) {

    afs = auto_fselector(
      fselector = fselector,
      learner = learners[[i]],
      resampling = inner_resampling,
      measure = measure,
      terminator = terminator,
      store_models = TRUE
    )

    task_subset = task$clone()$filter(outer_resampling$train_set(i))
    resampling = rsmp("insample")$instantiate(task_subset)

    data.table(
      iter = i,
      base_learner_id = learners[[i]]$id,
      base_learner = list(learners[[i]]),
      learner = list(afs),
      task = list(task_subset),
      resampling = list(resampling)
    )
  })

  design = grid[, list(learner, task, resampling)]

  bmr = benchmark(design, store_models = TRUE)

  # extract
  afss = bmr$score()$learner
  features = map(afss, function(afs) {
    afs$fselect_result$features[[1]]
  })

  n_features = map_int(afss, function(afs) {
    afs$fselect_result$n_features[[1]]
  })

  set(grid, j = "features", value = features)
  set(grid, j = "n_features", value = n_features)

  grid
}

if (FALSE) {
  task = tsk("sonar")
  learners = lrns(c("classif.rpart", "classif.rpart"))
  outer_resampling = rsmp("subsampling", repeats = 2)
  inner_resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  fselector = fs("random_search")
  terminator = trm("evals", n_evals = 10)

  ensemble_fselect(task, learners, outer_resampling, inner_resampling, fselector, terminator)

}
