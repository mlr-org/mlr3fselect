#' @title Ensemble Feature Selection
#'
#' @description
#' Ensemble feature selection using multiple learners.
#'
#' @param learners (list of [mlr3::Learner])\cr
#'  The learners to be used for feature selection.
#' @param outer_resampling ([mlr3::Resampling])\cr
#'  The outer resampling strategy.
#'  The number of iterations must match the number of learners.
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  The inner resampling strategy used by the [FSelector].
#'
#' @template param_fselector
#' @template param_task
#' @template param_measure
#' @template param_terminator
#' @template param_callbacks
#'
#' @export
#' @examples
#' \donttest{
#'
#'   ensemble_fselect(
#'     fselector = fs("random_search"),
#'     task = tsk("sonar"),
#'     learners = lrns(c("classif.rpart", "classif.featureless")),
#'     outer_resampling = rsmp("subsampling", repeats = 2),
#'     inner_resampling = rsmp("cv", folds = 3),
#'     measure = msr("classif.ce"),
#'     terminator = trm("evals", n_evals = 10)
#'   )
#' }
ensemble_fselect = function(fselector, task, learners, outer_resampling, inner_resampling, measure, terminator, callbacks = list()) {
  assert_task(task)
  assert_learners(as_learners(learners), task = task)
  assert_resampling(outer_resampling)

  # create fselector for each learner
  afss = map(learners, function(learner) {
    auto_fselector(
      fselector = fselector,
      learner = learner,
      resampling = inner_resampling,
      measure = measure,
      terminator = terminator,
      store_models = TRUE,
      callbacks = callbacks
    )
  })

  outer_resampling$instantiate(task)
  grid = map_dtr(seq(outer_resampling$iters), function(i) {

    # create task and resampling for each outer iteration
    task_subset = task$clone()$filter(outer_resampling$train_set(i))
    resampling = rsmp("insample")$instantiate(task_subset)

    data.table(
      iter = i,
      base_learner_id = map(learners, "id"),
      base_learner = learners,
      learner = afss,
      task = list(task_subset),
      resampling = list(resampling)
    )
  })

  design = grid[, c("learner", "task", "resampling"), with = FALSE]

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
