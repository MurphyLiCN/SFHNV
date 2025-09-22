#' Prepare data for SFHNV trees and forests
#'
#' Converts a data frame with outcome quantities into a numeric matrix representation
#' used by the Structural Forest for the Heterogeneous Newsvendor (SFHNV) estimators.
#'
#' @param data A `data.frame` containing demand `D`, quantile `Q`, and feature columns.
#' @return A list with prepared matrices (`X`), outcomes (`D`, `Q`), binary indicators (`z`),
#'   feature names, and the dimensions `n` and `p`.
#' @export
#' @examples
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), D = rnorm(100), Q = rnorm(100))
#' prep <- NW_prepare(data)
#' str(prep)
NW_prepare <- function(data) {
  stopifnot(is.data.frame(data), all(c("D", "Q") %in% colnames(data)))
  feat_names <- setdiff(colnames(data), c("D", "Q"))
  if (!length(feat_names)) stop("No feature columns besides D and Q")
  X <- as.matrix(data[, feat_names, drop = FALSE])
  storage.mode(X) <- "double"
  D <- as.double(data$D)
  Q <- as.double(data$Q)
  z <- as.integer(D <= Q)
  list(
    X = X, D = D, Q = Q, z = z,
    feature_names = feat_names, n = nrow(X), p = ncol(X)
  )
}
