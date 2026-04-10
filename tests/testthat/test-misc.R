# ──────────────────────────────────────────────────────────────────────────────
# as.graphNEL
# ──────────────────────────────────────────────────────────────────────────────

test_that("as.graphNEL drops tamat class", {
  m <- matrix(c(0, 1, 0, 0), nrow = 2, byrow = TRUE)
  rownames(m) <- colnames(m) <- c("A", "B")
  attr(m, "tamat_type") <- "pdag"
  class(m) <- c("tamat", "matrix")

  g <- as.graphNEL(m)
  expect_s4_class(g, "graphNEL")

  mat_from_g <- methods::as(g, "matrix")
  expect_equal(mat_from_g, t(unclass(m)), ignore_attr = TRUE)
})

# ──────────────────────────────────────────────────────────────────────────────
# translate_custom_test_to_bnlearn
# ──────────────────────────────────────────────────────────────────────────────

test_that("wraps function returning only p-value", {
  f <- function(x, y, data) {
    0.05
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  res <- wrapped("A", "B", NULL, data.frame(A = 1, B = 2))

  expect_type(res, "double")
  expect_length(res, 2)
  expect_true(is.na(res[1]))
  expect_equal(res[2], 0.05)
})

test_that("passes through statistic and p-value unchanged", {
  f <- function(x, y, data) {
    c(1.23, 0.04)
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  res <- wrapped("A", "B", NULL, data.frame(A = 1, B = 2))

  expect_equal(res, c(1.23, 0.04))
})

test_that("maps conditioning_set correctly", {
  f <- function(x, y, conditioning_set, data) {
    expect_equal(conditioning_set, c("Z1", "Z2"))
    0.1
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  wrapped("A", "B", c("Z1", "Z2"), data.frame())
})

test_that("maps suff_stat instead of data", {
  f <- function(x, y, suff_stat) {
    expect_true(is.data.frame(suff_stat))
    0.2
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  wrapped("A", "B", NULL, data.frame(A = 1))
})

test_that("uses z if user function expects z", {
  f <- function(x, y, z) {
    expect_equal(z, "Z")
    0.3
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  wrapped("A", "B", "Z", data.frame())
})

test_that("passes args when supported", {
  f <- function(x, y, data, args) {
    expect_equal(args$alpha, 0.05)
    0.01
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  wrapped("A", "B", NULL, data.frame(), args = list(alpha = 0.05))
})

test_that("does not pass args when not supported", {
  f <- function(x, y, data) {
    0.02
  }

  wrapped <- translate_custom_test_to_bnlearn(f)

  expect_silent(
    wrapped("A", "B", NULL, data.frame(), args = list(alpha = 0.05))
  )
})

test_that("errors on invalid return length", {
  f <- function(x, y, data) {
    c(1, 2, 3)
  }

  wrapped <- translate_custom_test_to_bnlearn(f)

  expect_error(
    wrapped("A", "B", NULL, data.frame()),
    "must return"
  )
})

test_that("works with minimal function signature", {
  f <- function(x, y) {
    0.5
  }

  wrapped <- translate_custom_test_to_bnlearn(f)
  res <- wrapped("A", "B", "Z", data.frame())

  expect_length(res, 2)
  expect_equal(res[2], 0.5)
})
