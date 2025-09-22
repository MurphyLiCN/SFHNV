#' Fit an SFHNV random forest
#'
#' @param data A data frame or output from [NW_prepare()].
#' @param honest_ratio Ratio of the estimation subsample to the splitting subsample.
#' @param min_size Minimum number of observations in each child node.
#' @param max_depth Maximum depth of each tree.
#' @param num_trees Number of trees to build.
#' @param feature_choose Strategy for selecting features at each split. One of
#'   `"sqrt"`, `"log2"`, `"third"`, or `"all"`.
#' @param parallel Logical; if `TRUE` and `future.apply` is available, build trees in parallel.
#' @param approximate Logical; if `TRUE`, limit candidate split points for speed.
#' @param max_candidates Maximum candidate split points per feature when `approximate = TRUE`.
#' @param leaf_round_digits Rounding control for demand samples in leaf CDF estimation.
#'
#' @return A list of SFHNV trees.
#' @export
#' @examples
#' data <- data.frame(x1 = rnorm(200), x2 = rnorm(200), D = rnorm(200), Q = rnorm(200))
#' forest <- build_random_forest(data, num_trees = 5, min_size = 20)
build_random_forest <- function(data, honest_ratio = 1, min_size = 50, max_depth = 50, num_trees = 100,
                                feature_choose = "sqrt", parallel = TRUE,
                                approximate = FALSE, max_candidates = 256,
                                leaf_round_digits = 1L) {
  prep <- if (is.list(data) && !is.null(data$X) && !is.null(data$D) && !is.null(data$Q)) data else NW_prepare(data)
  p <- prep$p
  mtry <- switch(feature_choose,
    sqrt = floor(sqrt(p)),
    log2 = floor(log2(p)),
    third = floor(p / 3),
    all = p,
    stop("Unsupported feature_choose")
  )
  mtry <- max(1L, min(mtry, p))

  trees <- .map(seq_len(num_trees), function(i) {
    sel_idx <- sample.int(p, size = mtry)
    NW_Tree(prep,
      honest_ratio = honest_ratio, min_size = min_size, max_depth = max_depth,
      features = sel_idx, approximate = approximate, max_candidates = max_candidates,
      leaf_round_digits = leaf_round_digits
    )
  }, parallel = parallel)
  trees
}

#' Predict SFHNV random forest point estimates
#'
#' @param forest A list of trees produced by [build_random_forest()].
#' @param observations Data frame of new observations.
#' @param trim_prop Optional trimming proportion used in the robust aggregation.
#' @param parallel Logical; if `TRUE` and `future.apply` is available, predict in parallel.
#'
#' @return Numeric vector of aggregated predictions.
#' @export
#' @examples
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), D = rnorm(100), Q = rnorm(100))
#' forest <- build_random_forest(data, num_trees = 3, min_size = 15)
#' predict_forest(forest, data)
predict_forest <- function(forest, observations, trim_prop = 0.05, parallel = TRUE) {
  preds_list <- .map(seq_along(forest), function(i) predict_tree(forest[[i]], observations),
    parallel = parallel
  )
  preds_mat <- do.call(cbind, preds_list)
  apply(preds_mat, 1, .robust_mean, trim_prop = trim_prop)
}

#' Predict SFHNV random forest conditional CDF values
#'
#' @param forest A list of trees produced by [build_random_forest()].
#' @param observations Data frame of new observations.
#' @param d_values Scalar or vector of demand thresholds.
#' @param parallel Logical; if `TRUE` and `future.apply` is available, predict in parallel.
#' @param agg Aggregation strategy across trees (`"mean"`, `"median"`, or `"trimmed"`).
#' @param trim_prop Trimming proportion when `agg = "trimmed"`.
#'
#' @return Numeric vector of CDF estimates.
#' @export
#' @examples
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), D = rnorm(100), Q = rnorm(100))
#' forest <- build_random_forest(data, num_trees = 3, min_size = 15)
#' predict_cdf_forest(forest, data, d_values = 0)
predict_cdf_forest <- function(forest, observations, d_values, parallel = TRUE,
                               agg = "mean", trim_prop = 0.05) {
  preds_list <- .map(seq_along(forest), function(i) predict_cdf_tree(forest[[i]], observations, d_values),
    parallel = parallel
  )
  preds_mat <- simplify2array(preds_list)
  if (agg == "mean") {
    rowMeans(preds_mat, na.rm = TRUE)
  } else if (agg == "median") {
    apply(preds_mat, 1, stats::median, na.rm = TRUE)
  } else {
    apply(preds_mat, 1, .robust_mean, trim_prop = trim_prop)
  }
}
