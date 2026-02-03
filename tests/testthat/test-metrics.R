test_that("Metrics compute the correct", {
  cg1 <- caugi::caugi(
    A %-->% B + C,
    B %-->% D
  )
  cg2 <- caugi::caugi(B %-->% A + C + D)

  expect_equal(precision(cg1, cg2, type = "adj"), 2 / 3)
  expect_equal(precision(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(recall(cg1, cg2, type = "adj"), 2 / 3)
  expect_equal(recall(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(specificity(cg1, cg2, type = "adj"), 2 / 3)
  expect_equal(specificity(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(false_omission_rate(cg1, cg2, type = "adj"), 1 / 3)
  expect_equal(false_omission_rate(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(fdr(cg1, cg2, type = "adj"), 1 / 3)
  expect_equal(fdr(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(npv(cg1, cg2, type = "adj"), 2 / 3)
  expect_equal(npv(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(f1_score(cg1, cg2, type = "adj"), 2 / 3)
  expect_equal(f1_score(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(shd(cg1, cg2), 3)
})

test_that("Confusion errors on non-caugi objects", {
  cg1 <- caugi::caugi(
    A %-->% B + C,
    B %-->% D
  )
  cg2 <- matrix(0, 2, 2)

  expect_error(
    confusion(cg1, cg2),
    "Both inputs must be caugi objects."
  )
})
