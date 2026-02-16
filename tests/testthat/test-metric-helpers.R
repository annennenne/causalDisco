# -------------------------------------------------
# adj_confusion tests
# -------------------------------------------------

test_that("adj_confusion returns perfect confusion for identical graphs", {
  true <- make_amat(3, list(c(1, 2), c(2, 1), c(2, 3), c(3, 2)))
  est <- true

  res <- adj_confusion(est, true)

  expect_equal(res$fp, 0)
  expect_equal(res$fn, 0)
  expect_gt(res$tp, 0)
})

test_that("adj_confusion detects missing adjacency as false negatives", {
  true <- make_amat(3, list(c(1, 2), c(2, 1)))
  est <- make_amat(3, list()) # empty graph

  res <- adj_confusion(est, true)

  expect_equal(res$tp, 0)
  expect_equal(res$fp, 0)
  expect_gt(res$fn, 0)
})

test_that("adj_confusion detects extra adjacency as false positives", {
  true <- make_amat(3, list())
  est <- make_amat(3, list(c(1, 2), c(2, 1)))

  res <- adj_confusion(est, true)

  expect_equal(res$tp, 0)
  expect_equal(res$fn, 0)
  expect_gt(res$fp, 0)
})

# -------------------------------------------------
# dir_confusion tests
# -------------------------------------------------

test_that("dir_confusion gives perfect scores for identical directed graphs", {
  true <- make_amat(3, list(c(1, 2), c(2, 3)))
  est <- true

  res <- dir_confusion(est, true)

  expect_equal(res$fp, 0)
  expect_equal(res$fn, 0)
  expect_gt(res$tp, 0)
})

test_that("dir_confusion penalizes reversed directions", {
  true <- make_amat(2, list(c(1, 2)))
  est <- make_amat(2, list(c(2, 1)))

  res <- dir_confusion(est, true)

  expect_equal(res$tp, 0)
  expect_gt(res$fp, 0)
  expect_gt(res$fn, 0)
})

test_that("dir_confusion penalizes undirected when direction is required", {
  true <- make_amat(2, list(c(1, 2)))
  est <- make_amat(2, list(c(1, 2), c(2, 1))) # undirected

  res <- dir_confusion(est, true)

  expect_equal(res$tp, 0)
  expect_equal(res$fp, 0)
  expect_gt(res$fn, 0)
})

test_that("dir_confusion penalizes directed when edge should be undirected", {
  true <- make_amat(2, list(c(1, 2), c(2, 1))) # undirected
  est <- make_amat(2, list(c(1, 2))) # directed

  res <- dir_confusion(est, true)

  expect_equal(res$tp, 0)
  expect_gt(res$fp, 0)
})

test_that("dir_confusion ignores non-adjacent edges", {
  true <- make_amat(3, list(c(1, 2)))
  est <- make_amat(3, list(c(2, 3)))

  res <- dir_confusion(est, true)

  expect_equal(res$tp, 0)
  expect_equal(res$fp, 0)
  expect_equal(res$fn, 0)
})
