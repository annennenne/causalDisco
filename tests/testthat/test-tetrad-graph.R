# ──────────────────────────────────────────────────────────────────────────────
# tetrad_graph()
# ──────────────────────────────────────────────────────────────────────────────

test_that("tetrad_graph() errors if x is not a single character string", {
  expect_error(tetrad_graph(1), "`x` must be a single character string")
  expect_error(
    tetrad_graph(c("a", "b")),
    "`x` must be a single character string"
  )
})

test_that("tetrad_graph() errors if adjacency matrix is not square", {
  bad_string <- "V1,V2\n0,1\n1,0\n1,1"
  expect_error(tetrad_graph(bad_string), "Adjacency matrix must be square")
})

test_that("tetrad_graph() parses a valid PAG string correctly", {
  x <- "V1,V2,V3\n0,2,0\n3,0,1\n0,3,0\n"
  g <- tetrad_graph(x)

  # Structure
  expect_s3_class(g, "tetrad_graph")
  expect_named(g, c("nodes", "amat"))

  # Nodes
  expect_equal(g$nodes, c("V1", "V2", "V3"))

  # amat properties
  expect_equal(dim(g$amat), c(3, 3))
  expect_equal(dimnames(g$amat), list(c("V1", "V2", "V3"), c("V1", "V2", "V3")))
  expect_true(storage.mode(g$amat) == "integer")
  expect_equal(g$amat[1, 2], 2L)
})

test_that("tetrad_graph() ignores blank lines", {
  x <- "V1,V2,V3\n0,2,0\n3,0,1\n\n0,3,0\n\n\n"
  g <- tetrad_graph(x)
  expect_equal(g$nodes, c("V1", "V2", "V3"))
  expect_equal(dim(g$amat), c(3, 3))
})
