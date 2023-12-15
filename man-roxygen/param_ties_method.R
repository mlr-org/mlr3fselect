#' @param ties_method (`character(1)`)\cr
#' The method to break ties when selecting sets while optimizing and when selecting the best set.
#' Can be one of `n_features`, `first`, `random`.
#' The option `n_features` (default) selects the feature set with the least features.
#' If there are multiple best feature sets with the same number of features, the first one is selected.
#' The `first` method returns the first added best feature set.
#' The `random` method returns a random feature set from the best feature sets.
#' Ignored if multiple measures are used.
