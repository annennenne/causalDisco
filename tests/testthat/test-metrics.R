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

  expect_equal(g1_score(cg1, cg2, type = "adj"), 2 / 3)
  expect_equal(g1_score(cg1, cg2, type = "dir"), 1 / 2)

  expect_equal(shd(cg1, cg2), 3)
})

test_that("evaluate works", {
  cg1 <- caugi::caugi(A %-->% B + C)
  cg2 <- caugi::caugi(B %-->% A + C)

  out <- evaluate(cg1, cg2)
  out_all <- evaluate(
    cg1,
    cg2,
    metrics = list(
      adj = c("precision", "recall"),
      dir = c("f1_score"),
      other = c("shd")
    )
  )

  expect_true(is.data.frame(out))
  expect_true(is.data.frame(out_all))
  expect_equal(ncol(out_all), 4)
})

test_that("evaluate errors on wrong metric", {
  cg1 <- caugi::caugi(A %-->% B + C)
  cg2 <- caugi::caugi(B %-->% A + C)
  expect_error(
    evaluate(
      cg1,
      cg2,
      metrics = list(
        adj = c("precision", "non_existing_metric"),
        dir = c("f1_score"),
        other = c("shd")
      )
    ),
    "Invalid adj metric(s): non_existing_metric",
    fixed = TRUE
  )
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

test_that("Confusion errors on non PDAG", {
  cg1 <- caugi::caugi(
    A %<->% B + C,
    B %-->% D
  )
  cg2 <- caugi::caugi(
    A %o->% B + C,
    B %-->% D
  )

  expect_error(
    confusion(cg1, cg2),
    "Confusion matrix only supports `-->` and `---` edges (PDAG).",
    fixed = TRUE
  )
})
