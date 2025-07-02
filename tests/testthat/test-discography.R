# discography tests
library(testthat)

# ──────────────────────────────────────────────────────────────────────────────
# discography() fails on unknown object
# ──────────────────────────────────────────────────────────────────────────────

test_that("discography fails on unknown object", {
  expect_error(discography(1:10), "Don't know")
  expect_error(discography("1-2-3"), "Don't know")
})

# ──────────────────────────────────────────────────────────────────────────────
# bnlearn
# ──────────────────────────────────────────────────────────────────────────────

test_that("conversion from empty bnlearn graph works", {
  e <- bnlearn::empty.graph(LETTERS[1:6])
  edge_tbl <- e$arcs |>
    as_tibble()
  edge_tbl$edge_type <- as.character(NULL)
  disco_tbl <- tibble::as_tibble(discography(e))
  expect_equal(disco_tbl, edge_tbl)
})

test_that("conversion from fully connected bnlearn graph works", {
  e <- bnlearn::complete.graph(LETTERS[1:6])
  edge_tbl <- e$arcs |>
    as_tibble()
  edge_tbl$edge_type <- "-->"
  disco_tbl <- tibble::as_tibble(discography(e))
  expect_equal(disco_tbl, edge_tbl)
})

test_that("conversion from bnlearn cpdag works", {
  e <- bnlearn::empty.graph(LETTERS[1:4])
  e <- bnlearn::set.arc(e, "A", "B")
  e <- bnlearn::set.arc(e, "C", "B")
  e <- bnlearn::set.arc(e, "D", "C")
  e <- bnlearn::set.arc(e, "A", "D")
  e <- bnlearn::cpdag(e)

  # cpdag edge tibble
  cpdag_edge_tbl <- tibble::tibble(
    from = c("A", "C", "A", "C"),
    to = c("B", "B", "D", "D"),
    edge_type = c("-->", "-->", "---", "---")
  )

  disco_tbl <- tibble::as_tibble(discography(e))

  expect_equal(disco_tbl, cpdag_edge_tbl)
})


# ──────────────────────────────────────────────────────────────────────────────
# tetrad_graph
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# graphNEL
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# pcAlgo, gAlgo
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# Essgraph (pcalg::ges)
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# fciAlgo, amat, amat.pag, amat.cpdag
# ──────────────────────────────────────────────────────────────────────────────
