#' Fit a Structural Forest Heterogeneous Newsvendor tree
#'
#' Builds an honest tree that estimates the structural parameter of the heterogeneous
#' newsvendor model using the SFHNV algorithm.
#'
#' @param data A data frame or output from [NW_prepare()].
#' @param honest_ratio Ratio of the estimation subsample to the splitting subsample.
#' @param min_size Minimum number of observations in each child node.
#' @param max_depth Maximum depth of the tree.
#' @param features Optional subset of features (names or indices) to consider at each split.
#' @param approximate Logical; if `TRUE`, limit candidate split points for speed.
#' @param max_candidates Maximum candidate split points per feature when `approximate = TRUE`.
#' @param leaf_round_digits Control the rounding of demand samples when fitting leaf CDFs.
#'   Use negative values to disable rounding.
#'
#' @return A list representing the fitted tree.
#' @export
#' @examples
#' data <- data.frame(x1 = rnorm(200), x2 = rnorm(200), D = rnorm(200), Q = rnorm(200))
#' tree <- NW_Tree(data, min_size = 20, max_depth = 5)
#' preds <- predict_tree(tree, data)
NW_Tree <- function(data, honest_ratio = 1, min_size = 50, max_depth = 50,
                    features = NULL, approximate = FALSE, max_candidates = 256,
                    leaf_round_digits = 1L) {
  prep <- if (is.list(data) && !is.null(data$X) && !is.null(data$D) && !is.null(data$Q)) {
    data
  } else {
    NW_prepare(data)
  }
  n <- prep$n
  feature_idx <- if (is.null(features)) {
    seq_len(prep$p)
  } else {
    if (is.numeric(features)) {
      as.integer(features)
    } else {
      match(features, prep$feature_names)
    }
  }
  feature_idx <- feature_idx[!is.na(feature_idx) & feature_idx >= 1L & feature_idx <= prep$p]
  if (!length(feature_idx)) stop("No valid features specified")

  train_pct <- honest_ratio / (1 + honest_ratio)
  train_n <- max(1L, floor(train_pct * n))
  idx <- sample.int(n)
  train_idx <- idx[seq_len(train_n)]
  leaf_idx <- idx[(train_n + 1L):n]
  if (train_n >= n) leaf_idx <- integer(0)

  mc <- if (approximate) max_candidates else NULL
  tree <- .build_tree_idx_prepared(
    prep, train_idx, feature_idx, min_size, 0L, max_depth,
    honest_ratio, mc
  )
  tree <- .update_leaves_idx_prepared(tree, prep, leaf_idx, round_digits = leaf_round_digits)
  tree
}

predict_single_matrix <- function(tree, x_row) {
  node <- tree
  while (is.null(node$prediction)) {
    j <- node$feature_index
    if (x_row[j] <= node$value) {
      node <- node$left
    } else {
      node <- node$right
    }
  }
  node$prediction
}

predict_tree_matrix <- function(tree, X_new) {
  n <- nrow(X_new)
  if (n == 0L) {
    return(numeric(0))
  }
  vapply(seq_len(n), function(i) predict_single_matrix(tree, X_new[i, , drop = FALSE]), numeric(1))
}

#' Predict SFHNV tree point estimates
#'
#' @param tree An object produced by [NW_Tree()].
#' @param observations Data frame of new observations containing the same features
#'   as the training data.
#'
#' @return Numeric vector of predicted structural parameters.
#' @export
#' @examples
#' data <- data.frame(x = rnorm(50), D = rnorm(50), Q = rnorm(50))
#' tree <- NW_Tree(data, min_size = 10, max_depth = 3)
#' predict_tree(tree, data)
predict_tree <- function(tree, observations) {
  stopifnot(is.data.frame(observations))
  feature_names <- if (!is.null(tree$meta$feature_names)) {
    tree$meta$feature_names
  } else {
    stop("Tree missing feature_names metadata; rebuild tree with NW_Tree")
  }
  X_new <- .features_matrix(observations, feature_names)
  predict_tree_matrix(tree, X_new)
}

predict_cdf_tree_matrix <- function(tree, X_new, d_values) {
  n <- nrow(X_new)
  if (n == 0L) {
    return(numeric(0))
  }
  if (length(d_values) == 1L) d_values <- rep(d_values, n)
  if (length(d_values) != n) stop("d_values must be length 1 or match nrow(X_new)")

  vapply(seq_len(n), function(i) {
    node <- tree
    xi <- X_new[i, ]
    while (is.null(node$prediction)) {
      j <- node$feature_index
      if (xi[j] <= node$value) node <- node$left else node <- node$right
    }
    if (is.null(node$leaf_cdf)) {
      return(NA_real_)
    }
    node$leaf_cdf(d_values[i])
  }, numeric(1))
}

#' Predict conditional CDF values from an SFHNV tree
#'
#' @param tree An object produced by [NW_Tree()].
#' @param observations Data frame of new observations containing the same features
#'   as the training data.
#' @param d_values Either a scalar demand threshold applied to all observations,
#'   or a numeric vector with one value per observation.
#'
#' @return Numeric vector of CDF values.
#' @export
#' @examples
#' data <- data.frame(x = rnorm(50), D = rnorm(50), Q = rnorm(50))
#' tree <- NW_Tree(data, min_size = 10, max_depth = 3)
#' predict_cdf_tree(tree, data, d_values = 0)
predict_cdf_tree <- function(tree, observations, d_values) {
  stopifnot(is.data.frame(observations))
  feature_names <- if (!is.null(tree$meta$feature_names)) {
    tree$meta$feature_names
  } else {
    stop("Tree missing feature_names metadata; rebuild tree with NW_Tree")
  }
  X_new <- .features_matrix(observations, feature_names)
  predict_cdf_tree_matrix(tree, X_new, d_values)
}
