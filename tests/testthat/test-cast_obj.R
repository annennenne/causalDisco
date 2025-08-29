library(testthat)

test_that("cast_obj() casts ScoreWrapper implementors", {
  skip_if_no_tetrad()

  # Try common score wrappers; use whichever is available
  score <- try(rJava::.jnew("edu/cmu/tetrad/algcomparison/score/SemBicScore"), silent = TRUE)
  if (inherits(score, "try-error")) {
    score <- try(rJava::.jnew("edu/cmu/tetrad/algcomparison/score/BDeuScore"), silent = TRUE)
  }
  if (inherits(score, "try-error")) {
    skip("No ScoreWrapper implementation available on classpath (SemBicScore/BDeuScore).")
  }

  out <- cast_obj(score)
  expect_true(rJava::.jinstanceof(out, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper"))
})

test_that("cast_obj() casts DataModel (DataSet)", {
  skip_if_no_tetrad()

  df <- data.frame(x = c(1.0, NA_real_), y = c(1L, 2L))
  ds <- rdata_to_tetrad(df)

  out <- cast_obj(ds)
  expect_true(rJava::.jinstanceof(out, "edu/cmu/tetrad/data/DataModel"))
})

test_that("cast_obj() casts IndependenceWrapper implementors", {
  skip_if_no_tetrad()

  # FisherZ is a common wrapper with a no-arg ctor in algcomparison
  indep <- try(rJava::.jnew("edu/cmu/tetrad/algcomparison/independence/FisherZ"), silent = TRUE)
  if (inherits(indep, "try-error")) {
    skip("No IndependenceWrapper implementation available on classpath (FisherZ).")
  }

  out <- cast_obj(indep)
  expect_true(rJava::.jinstanceof(out, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper"))
})

test_that("cast_obj() casts Graph implementors (EdgeListGraph)", {
  skip_if_no_tetrad()

  # EdgeListGraph usually has a no-arg constructor
  graph <- try(rJava::.jnew("edu/cmu/tetrad/graph/EdgeListGraph"), silent = TRUE)
  if (inherits(graph, "try-error")) {
    skip("EdgeListGraph not available or lacks no-arg constructor on this Tetrad build.")
  }

  out <- cast_obj(graph)
  expect_true(rJava::.jinstanceof(out, "edu/cmu/tetrad/graph/Graph"))
})

test_that("cast_obj() errors on unsupported Java objects", {
  skip_if_no_tetrad()

  s <- rJava::.jnew("java/lang/String", "nope")
  expect_error(cast_obj(s), "The Java object cannot be cast to a superclass by cast_obj.")
})
