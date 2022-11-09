#' @param resampling ([mlr3::Resampling])\cr
#'   Resampling that is used to evaluated the performance of the feature subsets.
#'   Uninstantiated resamplings are instantiated during construction so that all feature subsets are evaluated on the same data splits.
#'   Already instantiated resamplings are kept unchanged.
