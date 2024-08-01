#' @title Ensemble Feature Selection Result
#'
#' @name ensemble_fs_result
#'
#' @description
#' The `EnsembleFSResult` stores the results of ensemble feature selection.
#' It includes methods for evaluating the stability of the feature selection process and for ranking the selected features among others.
#' The function [ensemble_fselect()] returns an object of this class.
#'
#' @section S3 Methods:
#' * `as.data.table.EnsembleFSResult(x, benchmark_result = TRUE)`\cr
#' Returns a tabular view of the ensemble feature selection.\cr
#' [EnsembleFSResult] -> [data.table::data.table()]\cr
#'     * `x` ([EnsembleFSResult])
#'     * `benchmark_result` (`logical(1)`)\cr
#'       Whether to add the learner, task and resampling information from the benchmark result.
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
#'     measure = msr("classif.ce"),
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
#'   # returns the empirical pareto front (nfeatures vs error)
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
    #'  Column names should include `"resampling_iteration"`, `"learner_id"`, `"features"`
    #'  and `"n_features"`.
    #' @param features ([character()])\cr
    #'  The vector of features of the task that was used in the ensemble feature
    #'  selection.
    #' @param benchmark_result ([mlr3::BenchmarkResult])\cr
    #'  The benchmark result object.
    #' @param measure_id (`character(1)`)\cr
    #'  Column name of `"result"` that corresponds to the measure used.
    #' @param minimize (`logical(1)`)\cr
    #'  If `TRUE` (default), lower values of the measure correspond to higher performance.
    initialize = function(result, features, benchmark_result = NULL, measure_id,
                          minimize = TRUE) {
      assert_data_table(result)
      private$.measure_id = assert_string(measure_id, null.ok = FALSE)
      mandatory_columns = c("resampling_iteration", "learner_id", "features", "n_features")
      assert_names(names(result), must.include = c(mandatory_columns, measure_id))
      private$.result = result
      private$.features = assert_character(features, any.missing = FALSE, null.ok = FALSE)
      private$.minimize = assert_logical(minimize, null.ok = FALSE)

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
      catf(format(self))
      print(private$.result[, c("resampling_iteration", "learner_id", "n_features"), with = FALSE])
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Calculates the feature ranking.
    #'
    #' @details
    #' The feature ranking process is built on the following framework: models act as *voters*, features act as *candidates*, and voters select certain candidates (features).
    #' The primary objective is to compile these selections into a consensus ranked list of features, effectively forming a committee.
    #'
    #' For every feature a score is calculated, which depends on the `"method"` argument.
    #' The higher the score, the higher the rank of the feature.
    #' Most methods have a `"*_weighted"` version that outputs a weighted score.
    #' The weights used are equal to the performance scores of each voter/model (or the inverse scores if the measure is minimized).
    #' The un-weighted methods use same weights for all voters (equal to 1).
    #'
    #' The following methods are currently supported:
    #'
    #' - `"av"|"av_weighted"` (approval voting) selects the candidates that have the highest approval score, i.e. the features that appear the most often.
    #' This is the default feature ranking method.
    #' - `"sav"|"sav_weighted"` (satisfaction approval voting) selects the candidates that have a higher satisfaction score,in proportion to the size of the voters approval sets.
    #' Voters who approve more candidates contribute a lesser score to the individual approved candidates.
    #'
    #' @param method (`character(1)`)\cr
    #' The method to calculate the feature ranking.
    #'
    #' @return A [data.table::data.table] listing all the features, ordered by decreasing scores (depends on the `"method"`).
    #' An extra column `"norm_score"` is produced for methods for which the original scores (i.e. approval counts in the case of approval voting) can be normalized and interpreted as **selection probabilities**, see Meinshausen et al. (2010).
    feature_ranking = function(method = "av") {
      assert_choice(method, choices = c("av", "av_weighted", "sav", "sav_weighted"))

      # cached results
      if (!is.null(private$.feature_ranking[[method]])) {
        return(private$.feature_ranking[[method]])
      }

      # candidates => all features, voters => list of selected (best) features sets
      candidates = private$.features
      voters = private$.result$features

      # calculate weights
      use_weights = grepl(pattern = "weighted", x = method)
      if (use_weights) {
        # voter weights are the (inverse) scores
        scores = private$.result[, get(private$.measure_id)]
        weights = if (private$.minimize) 1 / scores else scores
      } else {
        # all voters are equal
        weights = rep(1, length(voters))
      }

      # calculate scores
      if (method == "av" || method == "av_weighted") {
        res = approval_voting(voters, candidates, weights)
      } else if (method == "sav" || method == "sav_weighted") {
        res = satisfaction_approval_voting(voters, candidates, weights)
      }

      private$.feature_ranking[[method]] = res
      private$.feature_ranking[[method]]
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
      measure_id = private$.measure_id
      minimize = private$.minimize

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
      measure_id = private$.measure_id
      minimize = private$.minimize

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

    #' @field measure (`character(1)`)\cr
    #' Returns the measure id used in the ensemble feature selection.
    measure = function(rhs) {
      assert_ro_binding(rhs)
      private$.measure_id
    }
  ),

  private = list(
    .result = NULL, # with no R6 classes
    .stability_global = NULL,
    .stability_learner = NULL,
    .feature_ranking = NULL,
    .features = NULL,
    .measure_id = NULL,
    .minimize = NULL
  )
)

#' @export
as.data.table.EnsembleFSResult = function(x,  ...) {
  x$result
}
