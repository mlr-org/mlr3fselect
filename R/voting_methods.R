# some collection of voting methods for feature ranking

# @param voters list of feature vectors (best features, each a subset of "candidates")
# @param candidates vector with ALL features
# @param weights vector of weights, 1-1 correspondence with voters
# @return data.table with 4 columns: features (mandatory), score, norm_score, borda_score (mandatory). Always features are ordered with decreasing `score` (or descreasing
# `borda_score` if a method returns only a ranking).

approval_voting = function(voters, candidates, weights) {
  # faster R version in case of equal weights
  if (all(weights == 1)) {
    count_tbl = sort(table(unlist(voters)), decreasing = TRUE)
    features_selected = names(count_tbl)
    features_not_selected = setdiff(candidates, features_selected)
    approval_counts = as.vector(count_tbl)

    res_fs = data.table(
      feature = features_selected,
      score = approval_counts,
      norm_score = approval_counts / length(voters)
    )

    res_fns = data.table(
      feature = features_not_selected,
      score = 0,
      norm_score = 0
    )

    res = rbindlist(list(res_fs, res_fns))
  } else {
    res = as.data.table(AV_rcpp(voters, candidates, weights))
    setorderv(res, cols = "score", order = -1)
  }

  res[, borda_score := (nrow(res) - .I) / (nrow(res) - 1)]
}

satisfaction_approval_voting = function(voters, candidates, weights) {
  res = as.data.table(SAV_rcpp(voters, candidates, weights))
  setorderv(res, cols = "score", order = -1)
  res[, borda_score := (nrow(res) - .I) / (nrow(res) - 1)]
}
