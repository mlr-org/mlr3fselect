#' @title Ensemble Feature Selection Result
#'
#' @name ensemble_fs_result
#'
#' @description
#' The `EnsembleFSResult` stores the results of ensemble feature selection.
#' It includes methods for evaluating the stability of the feature selection process and for ranking the selected features among others.
#'
#' Both functions [ensemble_fselect()] and [embedded_ensemble_fselect()] return an object of this class.
#'
#' @section S3 Methods:
#' * `as.data.table.EnsembleFSResult(x, benchmark_result = TRUE)`\cr
#' Returns a tabular view of the ensemble feature selection.\cr
#' [EnsembleFSResult] -> [data.table::data.table()]\cr
#'     * `x` ([EnsembleFSResult])
#'     * `benchmark_result` (`logical(1)`)\cr
#'       Whether to add the learner, task and resampling information from the benchmark result.
#' * `c(...)`\cr
#'   ([EnsembleFSResult], ...) -> [EnsembleFSResult]\cr
#'   Combines multiple [EnsembleFSResult] objects into a new [EnsembleFSResult].
#'
#' @references
#' `r format_bib("das1999", "meinshausen2010")`
#'
#' @export
#' @examples
#' \donttest{
#'   efsr = ensemble_fselect(
#'     fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
#'     task = tsk("sonar"),
#'     learners = lrns(c("classif.rpart", "classif.featureless")),
#'     init_resampling = rsmp("subsampling", repeats = 2),
#'     inner_resampling = rsmp("cv", folds = 3),
#'     inner_measure = msr("classif.ce"),
#'     measure = msr("classif.acc"),
#'     terminator = trm("none")
#'   )
#'
#'   # contains the benchmark result
#'   efsr$benchmark_result
#'
#'   # contains the selected features for each iteration
#'   efsr$result
#'
#'   # returns the stability of the selected features
#'   efsr$stability(stability_measure = "jaccard")
#'
#'   # returns a ranking of all features
#'   head(efsr$feature_ranking())
#'
#'   # returns the empirical pareto front, i.e. n_features vs measure (error)
#'   efsr$pareto_front()
#'
#'   # returns the knee points (optimal trade-off between n_features and performance)
#'   efsr$knee_points()
#'
#'   # change to use the inner optimization measure
#'   efsr$set_active_measure(which = "inner")
#'
#'   # Pareto front is calculated on the inner measure
#'   efsr$pareto_front()
#' }
EnsembleFSResult = R6Class("EnsembleFSResult",
  public = list(

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' The benchmark result.
    benchmark_result = NULL,

    #' @field man (`character(1)`)\cr
    #' Manual page for this object.
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param result ([data.table::data.table])\cr
    #'  The result of the ensemble feature selection.
    #'  Mandatory column names should include `"resampling_iteration"`, `"learner_id"`,
    #'  `"features"` and `"n_features"`.
    #'  A column named as `{measure$id}` (scores on the test sets) must also be
    #'  always present.
    #'  The column with the performance scores on the inner resampling of the train sets is not mandatory,
    #'  but note that it should be named as `{inner_measure$id}_inner` to distinguish from
    #'  the `{measure$id}`.
    #' @param features ([character()])\cr
    #'  The vector of features of the task that was used in the ensemble feature
    #'  selection.
    #' @param benchmark_result ([mlr3::BenchmarkResult])\cr
    #'  The benchmark result object.
    #' @param measure ([mlr3::Measure])\cr
    #'  The performance measure used to evaluate the learners on the test sets generated
    #'  during the ensemble feature selection process.
    #'  By default, this serves as the 'active' measure for the methods of this object.
    #'  The active measure can be updated using the `$set_active_measure()` method.
    #' @param inner_measure ([mlr3::Measure])\cr
    #'  The performance measure used to optimize and evaluate the learners during the inner resampling process of the training sets, generated as part of the ensemble feature selection procedure.
    initialize = function(
      result,
      features,
      benchmark_result = NULL,
      measure,
      inner_measure = NULL
      ) {
      assert_data_table(result)
      private$.measure = assert_measure(measure)
      private$.active_measure = "outer"
      measure_ids = c(private$.measure$id)
      if (!is.null(inner_measure)) {
        private$.inner_measure = assert_measure(inner_measure)
        # special end-fix required for inner measure
        measure_ids = c(measure_ids, sprintf("%s_inner", private$.inner_measure$id))
      }

      # the non-NULL measure ids should be defined as columns in the dt result
      mandatory_columns = c("resampling_iteration", "learner_id", "features",
                            "n_features", measure_ids)
      assert_names(names(result), must.include = mandatory_columns)
      private$.result = result
      private$.features = assert_character(features, any.missing = FALSE, null.ok = FALSE)

      # check that all feature sets are subsets of the task features
      assert_subset(unlist(result$features), private$.features)

      self$benchmark_result = if (!is.null(benchmark_result)) assert_benchmark_result(benchmark_result)

      self$man = "mlr3fselect::ensemble_fs_result"
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function(...) {
      catf("%s with %s learners and %s initial resamplings",
           format(self), self$n_learners, self$n_resamples)
      print(private$.result[, c("resampling_iteration", "learner_id", "n_features"), with = FALSE])
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Use this function to change the active measure.
    #'
    #' @param which (`character(1)`)\cr
    #'  Which [measure][mlr3::Measure] from the ensemble feature selection result
    #'  to use in methods of this object.
    #'  Should be either `"inner"` (optimization measure used in training sets)
    #'  or `"outer"` (measure used in test sets, default value).
    set_active_measure = function(which = "outer") {
      assert_choice(which, c("inner", "outer"))

      # check if `inner_measure` is an `mlr3::Measure`
      if (which == "inner" && is.null(private$.inner_measure)) {
        stop("No inner_measure was defined during initialization")
      }

      private$.active_measure = which
    },

    #' @description
    #' Combines a second [EnsembleFSResult] into the current object, modifying it **in-place**.
    #' If the second [EnsembleFSResult] (`efsr`) is `NULL`, the method returns the object unmodified.
    #'
    #' Both objects must have the same task features and `measure`.
    #' If the `inner_measure` differs between the objects or is `NULL` in either, it will be set to `NULL` in the combined object.
    #' Additionally, the `importance` column will be removed if it is missing in either object.
    #' If both objects contain a `benchmark_result`, these will be combined.
    #' Otherwise, the combined object will have a `NULL` value for `benchmark_result`.
    #'
    #' This method modifies the object by reference.
    #' To preserve the original state, explicitly `$clone()` the object beforehand.
    #' Alternatively, you can use the [c()] function, which internally calls this method.
    #'
    #' @param efsr ([EnsembleFSResult])\cr
    #'   A second [EnsembleFSResult] object to combine with the current object.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    combine = function(efsr) {
      if (!is.null(efsr)) {
        assert_class(efsr, "EnsembleFSResult")

        # Ensure both objects have the same task features
        assert_true(setequal(private$.features, get_private(efsr)$.features))

        # Ensure both objects have the same (outer) measure
        assert_true(private$.measure$id == get_private(efsr)$.measure$id)

        # Set inner measure to NULL if the measure ids are different or one of them is NULL
        inner_msr = private$.inner_measure
        inner_msr2 = get_private(efsr)$.inner_measure
        result2 = get_private(efsr)$.result
        if (is.null(inner_msr) || is.null(inner_msr2) || inner_msr$id != inner_msr2$id) {
          private$.inner_measure = NULL

          # Remove associated inner measure scores from results
          if (!is.null(inner_msr)) {
            private$.result[[sprintf("%s_inner", inner_msr$id)]] = NULL
          }
          if (!is.null(inner_msr2)) {
            result2[[sprintf("%s_inner", inner_msr2$id)]] = NULL
          }
        }

        # remove importance scores if missing in either object
        has_imp = "importance" %in% names(private$.result)
        has_imp2 = "importance" %in% names(result2)
        if (!has_imp || !has_imp2) {
          if (has_imp) private$.result[["importance"]] = NULL
          if (has_imp2) result2[["importance"]] = NULL
        }

        # Combine results from both objects
        private$.result = data.table::rbindlist(list(private$.result, result2), fill = FALSE)

        # Merge benchmark results if available in both objects
        has_bmr = !is.null(self$benchmark_result)
        has_bmr2 = !is.null(efsr$benchmark_result)
        if (has_bmr && has_bmr2) {
          self$benchmark_result = self$benchmark_result$combine(efsr$benchmark_result)
        } else {
          self$benchmark_result = NULL
        }
      }

      invisible(self)
    },

    #' @description
    #' Calculates the feature ranking via [fastVoteR::rank_candidates()].
    #'
    #' @details
    #' The feature ranking process is built on the following framework: models act as *voters*, features act as *candidates*, and voters select certain candidates (features).
    #' The primary objective is to compile these selections into a consensus ranked list of features, effectively forming a committee.
    #'
    #' For every feature a score is calculated, which depends on the `"method"` argument.
    #' The higher the score, the higher the ranking of the feature.
    #' Note that some methods output a feature ranking instead of a score per feature, so we always include **Borda's score**, which is method-agnostic, i.e. it can be used to compare the feature rankings across different methods.
    #'
    #' We shuffle the input candidates/features so that we enforce random tie-breaking.
    #' Users should set the same `seed` for consistent comparison between the different feature ranking methods and for reproducibility.
    #'
    #' @param method (`character(1)`)\cr
    #' The method to calculate the feature ranking. See [fastVoteR::rank_candidates()]
    #' for a complete list of available methods.
    #' Approval voting (`"av"`) is the default method.
    #' @param use_weights (`logical(1)`)\cr
    #' The default value (`TRUE`) uses weights equal to the performance scores
    #' of each voter/model (or the inverse scores if the measure is minimized).
    #' If `FALSE`, we treat all voters as equal and assign them all a weight equal to 1.
    #' @param committee_size (`integer(1)`)\cr
    #' Number of top selected features in the output ranking.
    #' This parameter can be used to speed-up methods that build a committee sequentially
    #' (`"seq_pav"`), by requesting only the top N selected candidates/features
    #' and not the complete feature ranking.
    #' @param shuffle_features (`logical(1)`)\cr
    #' Whether to shuffle the task features randomly before computing the ranking.
    #' Shuffling ensures consistent random tie-breaking across methods and prevents
    #' deterministic biases when features with equal scores are encountered.
    #' Default is `TRUE` and it's advised to set a seed before running this function.
    #' Set to `FALSE` if deterministic ordering of features is preferred (same as
    #' during initialization).
    #'
    #' @return A [data.table::data.table] listing all the features, ordered by decreasing scores (depends on the `"method"`). Columns are as follows:
    #' - `"feature"`: Feature names.
    #' - `"score"`: Scores assigned to each feature based on the selected method (if applicable).
    #' - `"norm_score"`: Normalized scores (if applicable), scaled to the range \eqn{[0,1]}, which can be loosely interpreted as **selection probabilities** (Meinshausen et al. (2010)).
    #' - `"borda_score"`: Borda scores for method-agnostic comparison, ranging in \eqn{[0,1]}, where the top feature receives a score of 1 and the lowest-ranked feature receives a score of 0.
    #' This column is always included so that feature ranking methods that output only rankings have also a feature-wise score.
    #'
    feature_ranking = function(method = "av", use_weights = TRUE, committee_size = NULL, shuffle_features = TRUE) {
      requireNamespace("fastVoteR")

      # candidates => all features, voters => list of selected (best) features sets
      candidates = private$.features
      voters = private$.result$features

      # calculate weights
      if (use_weights) {
        # voter weights are the (inverse) scores
        measure = self$measure # get active measure
        measure_id = ifelse(private$.active_measure == "inner",
                            sprintf("%s_inner", measure$id),
                            measure$id)

        scores = private$.result[, get(measure_id)]
        weights = if (measure$minimize) 1 / scores else scores
      } else {
        # all voters are equal
        weights = rep(1, length(voters))
      }

      # get consensus feature ranking
      res = fastVoteR::rank_candidates(
        voters = voters,
        candidates = candidates,
        weights = weights,
        committee_size = committee_size,
        method = method,
        borda_score = TRUE,
        shuffle_candidates = shuffle_features
      )

      setnames(res, "candidate", "feature")

      res
    },

    #' @description
    #' Calculates the stability of the selected features with the \CRANpkg{stabm} package.
    #' The results are cached.
    #' When the same stability measure is requested again with different arguments, the cache must be reset.
    #'
    #' @param stability_measure (`character(1)`)\cr
    #'  The stability measure to be used.
    #'  One of the measures returned by [stabm::listStabilityMeasures()] in lower case.
    #'  Default is `"jaccard"`.
    #' @param stability_args (`list`)\cr
    #'  Additional arguments passed to the stability measure function.
    #' @param global (`logical(1)`)\cr
    #'  Whether to calculate the stability globally or for each learner.
    #' @param reset_cache (`logical(1)`)\cr
    #'  If `TRUE`, the cached results are ignored.
    #'
    #' @return A `numeric()` value representing the stability of the selected features.
    #' Or a `numeric()` vector with the stability of the selected features for each learner.
    stability = function(
      stability_measure = "jaccard",
      stability_args = NULL,
      global = TRUE,
      reset_cache = FALSE
      ) {
      funs = stabm::listStabilityMeasures()$Name
      keys = tolower(gsub("stability", "", funs))
      assert_choice(stability_measure, choices = keys)
      assert_list(stability_args, null.ok = TRUE, names = "named")

      if (global) {
        # cached results
        if (!is.null(private$.stability_global[[stability_measure]]) && !reset_cache) {
          return(private$.stability_global[[stability_measure]])
        }

        fun = get(funs[which(stability_measure == keys)], envir = asNamespace("stabm"))
        private$.stability_global[[stability_measure]] = invoke(fun, features = private$.result$features, .args = stability_args)
        private$.stability_global[[stability_measure]]
      } else {
        # cached results
        if (!is.null(private$.stability_learner[[stability_measure]]) && !reset_cache) {
          return(private$.stability_learner[[stability_measure]])
        }

        fun = get(funs[which(stability_measure == keys)], envir = asNamespace("stabm"))

        learner_id = NULL
        tab = private$.result[, list(score = invoke(fun, features = .SD$features, .args = stability_args)), by = learner_id]
        private$.stability_learner[[stability_measure]] = set_names(tab$score, tab$learner_id)
        private$.stability_learner[[stability_measure]]
      }
    },

    #' @description
    #'
    #' This function identifies the **Pareto front** of the ensemble feature
    #' selection process, i.e., the set of points that represent the trade-off
    #' between the number of features and performance (e.g. classification error).
    #'
    #' @param type (`character(1)`)\cr
    #'  Specifies the type of Pareto front to return. See details.
    #'
    #' @details
    #' Two options are available for the Pareto front:
    #' - `"empirical"` (default): returns the empirical Pareto front.
    #' - `"estimated"`: the Pareto front points are estimated by fitting a linear model with the inversed of the number of features (\eqn{1/x}) as input and the associated performance scores as output.
    #'  This method is useful when the Pareto points are sparse and the front  assumes a convex shape if better performance corresponds to lower measure values (e.g. classification error), or a concave shape otherwise (e.g. classification accuracy).
    #'  The `estimated` Pareto front will include points for a number of features ranging from 1 up to the maximum number found in the empirical Pareto front.
    #'
    #' @return A [data.table::data.table] with columns the number of features and the performance that together form the Pareto front.
    pareto_front = function(type = "empirical") {
      assert_choice(type, choices =  c("empirical", "estimated"))
      result = private$.result
      measure = self$measure # get active measure
      measure_id = ifelse(private$.active_measure == "inner",
                          sprintf("%s_inner", measure$id),
                          measure$id)
      minimize = measure$minimize

      # Keep only n_features and performance scores
      cols_to_keep = c("n_features", measure_id)
      data = result[, cols_to_keep, with = FALSE]

      # Order data according to the measure
      data = if (minimize)
        data[order(n_features, -get(measure_id))]
      else
        data[order(n_features, get(measure_id))]

      # Initialize the Pareto front
      pf = data.table(n_features = numeric(0))
      pf[, (measure_id) := numeric(0)]

      # Initialize the best performance to a large number so
      # that the Pareto front has at least one point
      best_score = if (minimize) Inf else -Inf

      for (i in seq_row(data)) {
        # Determine the condition based on minimize
        if (minimize) {
          condition = data[[measure_id]][i] < best_score
        } else {
          condition = data[[measure_id]][i] > best_score
        }

        if (condition) {
          pf = rbind(pf, data[i])
          best_score = data[[measure_id]][i]
        }
      }

      if (type == "estimated") {
        # Transform the data (x => 1/x)
        n_features_inv = NULL
        pf[, n_features_inv := 1 / n_features]
        # remove edge cases where no features were selected
        pf = pf[n_features > 0]

        # Fit the linear model
        form = mlr3misc::formulate(lhs = measure_id, rhs = "n_features_inv")
        model = stats::lm(formula = form, data = pf)

        # Predict values using the model to create a smooth curve
        pf_pred = data.table(n_features = seq(1, max(data$n_features)))
        pf_pred[, n_features_inv := 1 / n_features]
        pf_pred[, (measure_id) := stats::predict(model, newdata = pf_pred)]
        pf_pred$n_features_inv = NULL
        pf = pf_pred
      }

      pf
    },

    #' @description
    #'
    #' This function implements various *knee* point identification (KPI) methods, which select points in the Pareto front, such that an optimal trade-off between performance and number of features is achieved.
    #' In most cases, only one such point is returned.
    #'
    #' @details
    #' The available KPI methods are:
    #'
    #' - `"NBI"` (default): The **Normal-Boundary Intersection** method is a geometry-based method which calculates the perpendicular distance of each point from the line connecting the first and last points of the Pareto front.
    #' The knee point is determined as the Pareto point with the maximum distance from this line, see Das (1999).
    #'
    #' @param method (`character(1)`)\cr
    #'  Type of method to use to identify the knee point. See details.
    #' @param type (`character(1)`)\cr
    #'  Specifies the type of Pareto front to use for the identification of the knee point.
    #'  See `pareto_front()` method for more details.
    #'
    #' @return A [data.table::data.table] with the knee point(s) of the Pareto front.
    knee_points = function(method = "NBI", type = "empirical") {
      assert_choice(method, choices = c("NBI"))
      assert_choice(type, choices = c("empirical", "estimated"))
      measure = self$measure # get active measure
      measure_id = ifelse(private$.active_measure == "inner",
                          sprintf("%s_inner", measure$id),
                          measure$id)
      minimize = measure$minimize

      pf = if (type == "empirical") self$pareto_front() else self$pareto_front(type = "estimated")

      # Scale the Pareto front data to (0-1) range
      nfeats = perf = dist_to_line = NULL
      pf_norm = pf[, list(
        nfeats = (n_features - min(n_features)) /(max(n_features) - min(n_features)),
        perf = (get(measure_id) - min(get(measure_id))) / (max(get(measure_id)) - min(get(measure_id)))
      )]

      if (minimize) {
        # The two edge points in the Pareto front are: (0,1) and (1,0)
        # They define the line (x + y - 1 = 0) and their distance is sqrt(2)
        pf_norm[, dist_to_line := abs(nfeats + perf - 1)/sqrt(2)]
      } else {
        # The two edge points in the Pareto front are: (0,0) and (1,1)
        # They define the line (y - x = 0) and their distance is sqrt(2)
        pf_norm[, dist_to_line := abs(nfeats - perf)/sqrt(2)]
      }

      # knee point is the one with the maximum distance
      knee_index = which_max(pf_norm[["dist_to_line"]], ties_method = "first")
      knee_point = pf[knee_index]

      knee_point
    }
  ),

  active = list(

    #' @field result ([data.table::data.table])\cr
    #' Returns the result of the ensemble feature selection.
    result = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$benchmark_result)) return(private$.result)
      tab = as.data.table(self$benchmark_result)[, c("task", "learner", "resampling"), with = FALSE]
      cbind(private$.result, tab)
    },

    #' @field n_learners (`numeric(1)`)\cr
    #' Returns the number of learners used in the ensemble feature selection.
    n_learners = function(rhs) {
      assert_ro_binding(rhs)
      uniqueN(private$.result$learner_id)
    },

    #' @field measure ([mlr3::Measure])\cr
    #' Returns the 'active' measure that is used in methods of this object.
    measure = function(rhs) {
      assert_ro_binding(rhs)

      if (private$.active_measure == "outer") {
        private$.measure
      } else {
        private$.inner_measure
      }
    },

    #' @field active_measure (`character(1)`)\cr
    #' Indicates the type of the active performance measure.
    #'
    #' During the ensemble feature selection process, the dataset is split into **multiple subsamples** (train/test splits) using an initial resampling scheme.
    #' So, performance can be evaluated using one of two measures:
    #'
    #' - `"outer"`: measure used to evaluate the performance on the test sets.
    #' - `"inner"`: measure used for optimization and to compute performance during inner resampling on the training sets.
    active_measure = function(rhs) {
      assert_ro_binding(rhs)
      private$.active_measure
    },

    #' @field n_resamples (`character(1)`)\cr
    #' Returns the number of times the task was initially resampled in the ensemble feature selection process.
    n_resamples = function(rhs) {
      assert_ro_binding(rhs)
      uniqueN(self$result$resampling_iteration)
    }
  ),

  private = list(
    .result = NULL, # with no R6 classes
    .stability_global = NULL,
    .stability_learner = NULL,
    .features = NULL,
    .measure = NULL,
    .inner_measure = NULL,
    .active_measure = NULL
  )
)

#' @export
as.data.table.EnsembleFSResult = function(x, ...) {
  x$result
}

#' @export
c.EnsembleFSResult = function(...) {
  efsrs = list(...)

  # Deep clone the first object for initialization
  init = efsrs[[1]]$clone(deep = TRUE)

  # If there's only one object, return it directly
  if (length(efsrs) == 1) {
    return(init)
  }

  # Combine the remaining objects
  rest = tail(efsrs, -1)
  Reduce(function(lhs, rhs) lhs$combine(rhs), rest, init = init)
}
