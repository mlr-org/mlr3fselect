# some collection of voting methods for feature ranking

# Parameters:
# @param voters list of feature vectors (best features, each a subset of "candidates")
# @param candidates vector with ALL features
# @param weights vector of weights, 1-1 correspondence with voters

approval_voting = function(voters, candidates, weights) {
  # faster R version in case of equal weights
  if (all(weights == 1)) {
    count_tbl = sort(table(unlist(voters)), decreasing = TRUE)
    features_selected = names(count_tbl)
    features_not_selected = setdiff(candidates, features_selected)

    res_fs = data.table(
      feature = features_selected,
      score = as.vector(count_tbl) / length(voters)
    )

    res_fns = data.table(
      feature = features_not_selected,
      score = 0
    )

    res = rbindlist(list(res_fs, res_fns))
  } else {
    score = NULL # fix data.table note
    as.data.table(
      rcpp_approval_voting(voters, candidates, weights)
    )[order(-rank(score))]
  }
}
