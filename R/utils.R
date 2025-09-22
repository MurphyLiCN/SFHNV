#' Internal helper utilities
#'
#' These functions support the Structural Forest for the Heterogeneous Newsvendor
#' estimators and are not exported.
#' @name SFHNV-internal
#' @keywords internal
NULL

.robust_mean <- function(x, trim_prop = 0.0) {
  n <- length(x)
  if (!is.finite(trim_prop) || trim_prop <= 0) {
    return(mean(x))
  }
  k <- floor(trim_prop * n)
  if (k == 0) {
    return(mean(x))
  }
  xs <- sort(x)
  mean(xs[(k + 1):(n - k)])
}

.map <- function(X, FUN, parallel = TRUE) {
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(X, FUN, future.seed = TRUE)
  } else {
    lapply(X, FUN)
  }
}

.I_bar_from_counts <- function(sum_leq, n) {
  I_raw <- sum_leq / n
  ifelse(I_raw == 0, 0.5 / n, ifelse(I_raw == 1, 1 - 0.5 / n, I_raw))
}

.gamma_hat_from_I <- function(I_bar) (1 / I_bar) - 1

.variance_from_I <- function(I_bar, n) (1 - I_bar) / (n * I_bar^3)

.features_matrix <- function(observations, feature_names) {
  stopifnot(is.data.frame(observations))
  X <- as.matrix(observations[, feature_names, drop = FALSE])
  storage.mode(X) <- "double"
  X
}

.is_integerish <- function(x, tol = 1e-8) {
  all(is.finite(x)) && all(abs(x - round(x)) <= tol)
}

.safe_sd <- function(x) {
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 1e-12) 1e-6 else s
}

.fit_discrete_dist <- function(x) {
  x <- as.double(round(x))
  n <- length(x)
  mu <- mean(x)
  v <- stats::var(x)

  ll_pois <- sum(stats::dpois(x, lambda = max(mu, 1e-8), log = TRUE))
  k_pois <- 1L

  if (is.finite(v) && v > mu + 1e-6) {
    p <- mu / v
    r <- mu^2 / (v - mu)
    p <- min(max(p, 1e-8), 1 - 1e-8)
    r <- max(r, 1e-6)
    ll_nb <- sum(stats::dnbinom(x, size = r, prob = p, log = TRUE))
    k_nb <- 2L
  } else {
    ll_nb <- -Inf
    k_nb <- 2L
  }

  bic_pois <- -2 * ll_pois + k_pois * log(max(n, 1))
  bic_nb <- -2 * ll_nb + k_nb * log(max(n, 1))
  if (bic_nb + 1e-9 < bic_pois) {
    list(
      type = "nbinom",
      params = list(size = r, prob = p),
      cdf = function(t) stats::pnbinom(floor(t), size = r, prob = p)
    )
  } else {
    list(
      type = "poisson",
      params = list(lambda = mu),
      cdf = function(t) stats::ppois(floor(t), lambda = mu)
    )
  }
}

.fit_continuous_dist <- function(x) {
  n <- length(x)
  mu <- mean(x)
  sd <- .safe_sd(x)
  ll_norm <- sum(stats::dnorm(x, mean = mu, sd = sd, log = TRUE))
  k_norm <- 2L

  all_pos <- all(is.finite(x)) && min(x) > 0
  if (all_pos) {
    lx <- log(x)
    mlog <- mean(lx)
    slog <- .safe_sd(lx)
    ll_logn <- sum(stats::dlnorm(x, meanlog = mlog, sdlog = slog, log = TRUE))
    k_logn <- 2L
  } else {
    ll_logn <- -Inf
    k_logn <- 2L
  }

  bic_norm <- -2 * ll_norm + k_norm * log(max(n, 1))
  bic_logn <- -2 * ll_logn + k_logn * log(max(n, 1))

  choose_norm <- (bic_norm + 1e-9 < bic_logn)

  close_bic <- abs(bic_norm - bic_logn) <= 2
  if (!is.finite(ll_norm) || sd < 1e-8 || n < 10) {
    return(.fit_kde_cdf(x))
  }
  if (!choose_norm && !all_pos) {
    choose_norm <- TRUE
  }
  if (close_bic && n < 30) {
    return(.fit_kde_cdf(x))
  }
  if (choose_norm) {
    list(
      type = "normal",
      params = list(mean = mu, sd = sd),
      cdf = function(t) stats::pnorm(t, mean = mu, sd = sd)
    )
  } else {
    lx <- log(x)
    mlog <- mean(lx)
    slog <- .safe_sd(lx)
    list(
      type = "lognormal",
      params = list(meanlog = mlog, sdlog = slog),
      cdf = function(t) stats::plnorm(pmax(t, 0), meanlog = mlog, sdlog = slog)
    )
  }
}

.fit_kde_cdf <- function(x) {
  dens <- stats::density(x, kernel = "gaussian", n = 512)
  xg <- dens$x
  yg <- pmax(dens$y, 0)
  dx <- mean(diff(xg))
  cdfg <- cumsum(yg) * dx
  cdfg <- pmin(1, pmax(0, cdfg / max(cdfg, 1e-12)))
  cdf_fun <- stats::approxfun(xg, cdfg, yleft = 0, yright = 1, rule = 2)
  list(
    type = "kde",
    params = list(x = xg, cdf = cdfg),
    cdf = function(t) cdf_fun(t)
  )
}

.build_leaf_cdf <- function(demands) {
  demands <- as.double(demands)
  n <- length(demands)
  if (n <= 1L || anyNA(demands)) {
    med <- stats::median(demands, na.rm = TRUE)
    return(list(type = "empirical", params = list(), cdf = function(t) as.numeric(t >= med)))
  }
  if (.is_integerish(demands)) {
    return(.fit_discrete_dist(demands))
  }
  .fit_continuous_dist(demands)
}

.select_candidates <- function(x_sorted, min_size, n, max_candidates = NULL) {
  changes <- which(x_sorted[-n] < x_sorted[-1])
  if (!length(changes)) {
    return(integer(0))
  }
  valid <- changes[changes >= min_size & changes <= (n - min_size)]
  if (!length(valid)) {
    return(integer(0))
  }
  if (!is.null(max_candidates) && length(valid) > max_candidates) {
    sel <- unique(round(seq(1, length(valid), length.out = max_candidates)))
    valid[sel]
  } else {
    valid
  }
}

.best_split_feature_idx_prepared <- function(prep, idx, j, min_size, honest_ratio,
                                             max_candidates = NULL) {
  n <- length(idx)
  if (n < 2L * min_size) {
    return(NULL)
  }
  x <- prep$X[idx, j]
  ord <- order(x, method = "radix")
  x_sorted <- x[ord]
  z <- prep$z[idx][ord]
  cum_z <- cumsum(z)
  total_z <- cum_z[n]

  candidates <- .select_candidates(x_sorted, min_size, n, max_candidates)
  if (!length(candidates)) {
    return(NULL)
  }

  left_n <- candidates
  right_n <- n - candidates
  left_z <- cum_z[candidates]
  right_z <- total_z - left_z

  I_left <- .I_bar_from_counts(left_z, left_n)
  I_right <- .I_bar_from_counts(right_z, right_n)
  g_left <- .gamma_hat_from_I(I_left)
  g_right <- .gamma_hat_from_I(I_right)
  v_left <- .variance_from_I(I_left, left_n)
  v_right <- .variance_from_I(I_right, right_n)
  emse_left <- -g_left^2 + (1 + honest_ratio) * v_left
  emse_right <- -g_right^2 + (1 + honest_ratio) * v_right
  weighted_emse <- (left_n * emse_left + right_n * emse_right) / n

  jmin <- which.min(weighted_emse)
  s <- candidates[jmin]
  best_value <- x_sorted[s]
  left_idx <- idx[ord[seq_len(s)]]
  right_idx <- idx[ord[(s + 1):n]]
  list(
    feature_index = j, value = best_value, emse = weighted_emse[jmin],
    left_idx = left_idx, right_idx = right_idx
  )
}

.find_best_split_idx_prepared <- function(prep, idx, feature_idx, min_size, honest_ratio,
                                          max_candidates = NULL) {
  best <- NULL
  best_emse <- Inf
  for (j in feature_idx) {
    s <- .best_split_feature_idx_prepared(prep, idx, j, min_size, honest_ratio, max_candidates)
    if (!is.null(s) && is.finite(s$emse) && s$emse < best_emse) {
      best <- s
      best_emse <- s$emse
    }
  }
  best
}

.build_tree_idx_prepared <- function(prep, idx, feature_idx, min_size, depth, max_depth,
                                     honest_ratio, max_candidates) {
  n <- length(idx)
  if (n < 2L * min_size || depth >= max_depth) {
    return(list(
      prediction = NA_real_, leaf_cdf = NULL, n = n,
      meta = list(feature_names = prep$feature_names)
    ))
  }

  split <- .find_best_split_idx_prepared(
    prep, idx, feature_idx, min_size, honest_ratio,
    max_candidates
  )
  if (is.null(split)) {
    return(list(
      prediction = NA_real_, leaf_cdf = NULL, n = n,
      meta = list(feature_names = prep$feature_names)
    ))
  }

  left_node <- .build_tree_idx_prepared(
    prep, split$left_idx, feature_idx, min_size,
    depth + 1L, max_depth, honest_ratio, max_candidates
  )
  right_node <- .build_tree_idx_prepared(
    prep, split$right_idx, feature_idx, min_size,
    depth + 1L, max_depth, honest_ratio, max_candidates
  )
  list(
    feature_index = split$feature_index,
    value = split$value,
    left = left_node,
    right = right_node,
    meta = list(feature_names = prep$feature_names)
  )
}

.update_leaves_idx_prepared <- function(node, prep, leaf_idx, round_digits = 1L) {
  if (is.null(node$feature_index)) {
    n <- length(leaf_idx)
    if (n <= 0L) {
      node$prediction <- NA_real_
      node$leaf_cdf <- NULL
      node$n <- 0L
      return(node)
    }
    sum_leq <- sum(prep$z[leaf_idx])
    I_bar <- .I_bar_from_counts(sum_leq, n)
    node$prediction <- .gamma_hat_from_I(I_bar)
    d_samp <- prep$D[leaf_idx]
    if (.is_integerish(d_samp)) {
      d_use <- round(d_samp)
    } else if (is.finite(round_digits) && !is.na(round_digits) && round_digits < 0) {
      d_use <- d_samp
    } else if (is.finite(round_digits) && !is.na(round_digits) && round_digits == 0L) {
      d_use <- round(d_samp)
    } else {
      d_use <- as.double(d_samp)
    }
    leaf_fit <- .build_leaf_cdf(d_use)
    node$leaf_cdf <- leaf_fit$cdf
    node$leaf_info <- list(type = leaf_fit$type, params = leaf_fit$params)
    node$n <- n
    return(node)
  }
  j <- node$feature_index
  v <- node$value
  li <- leaf_idx[prep$X[leaf_idx, j] <= v]
  ri <- leaf_idx[prep$X[leaf_idx, j] > v]
  node$left <- .update_leaves_idx_prepared(node$left, prep, li, round_digits)
  node$right <- .update_leaves_idx_prepared(node$right, prep, ri, round_digits)
  node
}
