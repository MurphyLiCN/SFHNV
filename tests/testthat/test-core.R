test_that("NW_prepare returns expected components", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    D = rnorm(50),
    Q = rnorm(50)
  )
  prep <- NW_prepare(df)
  expect_equal(prep$n, nrow(df))
  expect_equal(prep$p, ncol(df) - 2)
  expect_true(is.matrix(prep$X))
  expect_true(all(c("x1", "x2") %in% prep$feature_names))
})

test_that("NW_Tree produces finite predictions", {
  set.seed(456)
  df <- data.frame(
    x1 = rnorm(120),
    x2 = rnorm(120),
    D = rnorm(120),
    Q = rnorm(120)
  )
  tree <- NW_Tree(df, min_size = 10, max_depth = 4)
  preds <- predict_tree(tree, df)
  expect_equal(length(preds), nrow(df))
  expect_true(all(is.finite(preds)))
})

test_that("Random forest predictions are aggregated correctly", {
  set.seed(789)
  df <- data.frame(
    x1 = rnorm(150),
    x2 = rnorm(150),
    D = rnorm(150),
    Q = rnorm(150)
  )
  forest <- build_random_forest(df, num_trees = 5, min_size = 10, max_depth = 4, parallel = FALSE)
  preds <- predict_forest(forest, df, trim_prop = 0.1, parallel = FALSE)
  expect_equal(length(preds), nrow(df))
  expect_true(all(is.finite(preds)))
})

test_that("Forest CDF predictions lie in [0, 1]", {
  set.seed(101)
  df <- data.frame(
    x1 = rnorm(80),
    x2 = rnorm(80),
    D = rnorm(80),
    Q = rnorm(80)
  )
  forest <- build_random_forest(df, num_trees = 3, min_size = 8, max_depth = 4, parallel = FALSE)
  cdf_vals <- predict_cdf_forest(forest, df, d_values = 0, parallel = FALSE)
  expect_equal(length(cdf_vals), nrow(df))
  expect_true(all(is.finite(cdf_vals)))
  expect_true(all(cdf_vals >= 0 & cdf_vals <= 1))
})
