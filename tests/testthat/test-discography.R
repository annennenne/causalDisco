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
    tibble::as_tibble()
  edge_tbl$edge_type <- as.character(NULL)
  disco_tbl <- tibble::as_tibble(discography(e))
  expect_equal(disco_tbl, edge_tbl)
})

test_that("conversion from fully connected bnlearn graph works", {
  e <- bnlearn::complete.graph(LETTERS[1:6])
  edge_tbl <- e$arcs |>
    tibble::as_tibble() |>
    dplyr::arrange(
      factor(from, levels = LETTERS[1:6]),
      factor(to, levels = LETTERS[1:6])
    )
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
    from = c("A", "A", "C", "C"),
    to = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  )

  disco_tbl <- tibble::as_tibble(discography(e))

  expect_equal(disco_tbl, cpdag_edge_tbl)
})


# ──────────────────────────────────────────────────────────────────────────────
# tetrad_graph
# ──────────────────────────────────────────────────────────────────────────────

build_tetrad_graph <- function(nodes, amat) {
  mode(amat) <- "integer"
  class(amat) <- c("amat.pag", "matrix")
  if (nrow(amat) != ncol(amat)) {
    stop("Adjacency matrix must be square")
  }
  dimnames(amat) <- list(nodes, nodes)

  structure(list(nodes = nodes, amat = amat),
    class = "tetrad_graph"
  )
}

test_that("conversion from empty tetrad graph works", {
  nodes <- LETTERS[1:6]
  e <- build_tetrad_graph(nodes, matrix(0, nrow = 6, ncol = 6))
  edge_tbl <- tibble::tibble(
    from = character(0),
    to = character(0),
    edge_type = character(0)
  )
  disco_tbl <- tibble::as_tibble(discography(e))
  expect_equal(disco_tbl, edge_tbl)
})

test_that("conversion from fully connected tetrad graph works", {
  nodes <- LETTERS[1:6]
  amat <- matrix(0, 6, 6)
  amat[lower.tri(amat)] <- 3
  amat[upper.tri(amat)] <- 2
  dimnames(amat) <- list(nodes, nodes)

  e <- build_tetrad_graph(nodes, amat)
  edge_tbl <- tibble::tibble(
    from = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "D", "D", "E"),
    to = c("B", "C", "D", "E", "F", "C", "D", "E", "F", "D", "E", "F", "E", "F", "F"),
    edge_type = rep("-->", sum(1:5))
  )
  disco_tbl <- tibble::as_tibble(discography(e))
  expect_equal(disco_tbl, edge_tbl)
})

test_that("conversion from tetrad pag works", {
  nodes <- LETTERS[1:4]
  amat <- matrix(0, nrow = 4, ncol = 4)
  amat[1, 2] <- 2
  amat[2, 1] <- 3
  amat[1, 3] <- 1
  amat[3, 1] <- 2
  amat[2, 3] <- 3
  amat[3, 2] <- 1
  amat[2, 4] <- 1
  amat[4, 2] <- 2
  amat[3, 4] <- 3
  amat[4, 3] <- 1

  e <- build_tetrad_graph(nodes, amat)

  pag_edge_tbl <- tibble::tibble(
    from = c("A", "C", "C", "D", "D"),
    to = c("B", "A", "B", "B", "C"),
    edge_type = c("-->", "o->", "--o", "o->", "--o")
  )

  disco_tbl <- tibble::as_tibble(discography(e))

  expect_equal(disco_tbl, pag_edge_tbl)
})

test_that("conversion from tetrad cpdag works", {
  nodes <- LETTERS[1:4]
  amat <- matrix(0,
    nrow = 4, ncol = 4,
    dimnames = list(nodes, nodes)
  )

  # A  -->  B
  amat["A", "B"] <- 2 # arrow at B   (column mark = 2)
  amat["B", "A"] <- 3 # tail  at A   (column mark = 3)

  # C  -->  B
  amat["C", "B"] <- 2
  amat["B", "C"] <- 3

  # A  ---  D
  amat["A", "D"] <- 3
  amat["D", "A"] <- 3

  # C ---  D
  amat["C", "D"] <- 3
  amat["D", "C"] <- 3

  e <- build_tetrad_graph(nodes, amat)

  cpdag_edge_tbl <- tibble::tibble(
    from      = c("A", "A", "C", "C"),
    to        = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  )

  disco_tbl <- tibble::as_tibble(discography(e))

  expect_equal(disco_tbl, cpdag_edge_tbl)
})


# ──────────────────────────────────────────────────────────────────────────────
# graphNEL
# ──────────────────────────────────────────────────────────────────────────────
test_that("conversion from empty graphNEL works", {
  e <- new("graphNEL", edgemode = "directed")
  edge_tbl <- tibble::tibble(
    from = character(0),
    to = character(0),
    edge_type = character(0)
  )
  disco_tbl <- tibble::as_tibble(discography(e))
  expect_equal(disco_tbl, edge_tbl)
})

test_that("conversion from fully connected graphNEL works", {
  e <- new("graphNEL", nodes = LETTERS[1:6], edgemode = "directed")
  from <- c(
    "A", "A", "A", "A", "A",
    "B", "B", "B", "B",
    "C", "C", "C",
    "D", "D",
    "E"
  )
  to <- c(
    "B", "C", "D", "E", "F",
    "C", "D", "E", "F",
    "D", "E", "F",
    "E", "F",
    "F"
  )

  ft <- cbind(from, to)

  # build the directed graphNEL (no weights: W = NULL)
  e <- graph::ftM2graphNEL(ft, W = NULL, edgemode = "directed")

  edge_tbl <- tibble::tibble(
    from = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "D", "D", "E"),
    to = c("B", "C", "D", "E", "F", "C", "D", "E", "F", "D", "E", "F", "E", "F", "F"),
    edge_type = rep("-->", sum(1:5))
  )

  disco_tbl <- tibble::as_tibble(discography(e))

  expect_equal(disco_tbl, edge_tbl)
})

testthat::test_that("conversion from graphNEL cpdag works", {
  # build graphNEL with directed edgemode
  nodes <- LETTERS[1:4]
  g <- methods::new("graphNEL", nodes = nodes, edgemode = "directed")

  # directed edges
  g <- graph::addEdge("A", "B", g) # A --> B
  g <- graph::addEdge("C", "B", g) # C --> B

  # undirected edges represented by arcs both ways
  g <- graph::addEdge("A", "D", g) # A --- D (first direction)
  g <- graph::addEdge("D", "A", g) # A --- D (second direction)
  g <- graph::addEdge("C", "D", g) # C --- D (first direction)
  g <- graph::addEdge("D", "C", g) # C --- D (second direction)

  # expected edge table
  cpdag_edge_tbl <- tibble::tibble(
    from      = c("A", "A", "C", "C"),
    to        = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  )

  disco_tbl <- tibble::as_tibble(discography(g))

  testthat::expect_equal(disco_tbl, cpdag_edge_tbl)
})

testthat::test_that("conversion from fully-connected undirected graphNEL works", {
  # build a 4-node, fully connected, undirected graphNEL
  nodes <- LETTERS[1:4]
  g <- methods::new("graphNEL", nodes = nodes, edgemode = "undirected")

  # add every unordered pair exactly once
  pair_mat <- utils::combn(nodes, 2)
  for (k in seq_len(ncol(pair_mat))) {
    g <- graph::addEdge(pair_mat[1, k], pair_mat[2, k], g) # X --- Y
  }

  # expected edge table (6 undirected edges in alphabetical order)
  cpdag_edge_tbl <- tibble::tibble(
    from      = c("A", "A", "A", "B", "B", "C"),
    to        = c("B", "C", "D", "C", "D", "D"),
    edge_type = rep("---", 6)
  )

  disco_tbl <- tibble::as_tibble(discography(g))

  testthat::expect_equal(disco_tbl, cpdag_edge_tbl)
})

# ──────────────────────────────────────────────────────────────────────────────
# pcAlgo
# ──────────────────────────────────────────────────────────────────────────────
set.seed(42)
p <- 4
n <- 500
Sigma <- matrix(0.6, p, p)
diag(Sigma) <- 1
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- LETTERS[1:p]

# run the (deterministic) PC-stable algorithm
suffStat <- list(C = stats::cor(X), n = n)
pc_fit <- pcalg::pc(
  suffStat  = suffStat,
  indepTest = pcalg::gaussCItest,
  alpha     = 0.01,
  labels    = colnames(X),
  verbose   = FALSE
)

testthat::test_that("conversion from pcAlgo works", {
  # discography on the pcAlgo object
  disco_tbl <- tibble::as_tibble(discography(pc_fit))

  # expected edge table
  expected_tbl <- tibble::tibble(
    from      = c("A", "A", "A", "B", "B", "C"),
    to        = c("B", "C", "D", "C", "D", "D"),
    edge_type = rep("---", 6)
  )

  testthat::expect_equal(disco_tbl, expected_tbl)
})

testthat::test_that("conversion from pcAlgo is the same as the conversion from the graph", {
  # discography on the pcAlgo object versus its graph slot
  disco_tbl <- tibble::as_tibble(discography(pc_fit))
  reference_tbl <- tibble::as_tibble(discography(pc_fit@graph))

  testthat::expect_equal(disco_tbl, reference_tbl)
})

testthat::test_that("conversion from pcAlgo works", {
  # discography on the pcAlgo object
  disco_tbl <- tibble::as_tibble(discography(pc_fit))

  # expected edge table
  expected_tbl <- tibble::tibble(
    from      = c("A", "A", "A", "B", "B", "C"),
    to        = c("B", "C", "D", "C", "D", "D"),
    edge_type = rep("---", 6)
  )

  testthat::expect_equal(disco_tbl, expected_tbl)
})

# ──────────────────────────────────────────────────────────────────────────────
# Essgraph (pcalg::ges)
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("conversion from empty EssGraph works", {
  nodes <- LETTERS[1:4]
  ess <- list(
    .nodes     = nodes,
    .in.edges  = replicate(length(nodes), integer(0), simplify = FALSE)
  )
  class(ess) <- "EssGraph"

  edge_tbl <- tibble::tibble(
    from      = character(0),
    to        = character(0),
    edge_type = character(0)
  )

  disco_tbl <- tibble::as_tibble(discography(ess))

  testthat::expect_equal(disco_tbl, edge_tbl)
})

testthat::test_that("conversion from EssGraph works", {
  nodes <- LETTERS[1:4] # A, B, C, D

  ## parent lists (indices into 'nodes')
  ## A : no parents
  ## B : A
  ## C : A, B
  ## D : B
  ess <- list(
    .nodes = nodes,
    .in.edges = list(
      integer(0), # A
      1L, # B <- A
      c(1L, 2L), # C <- A,B
      2L # D <- B
    )
  )
  class(ess) <- "EssGraph"

  edge_tbl <- tibble::tibble(
    from      = c("A", "A", "B", "B"),
    to        = c("B", "C", "C", "D"),
    edge_type = rep("-->", 4)
  )

  disco_tbl <- tibble::as_tibble(discography(ess))

  testthat::expect_equal(disco_tbl, edge_tbl)
})

testthat::test_that("conversion from EssGraph produced by pcalg::ges works", {
  # sim data
  set.seed(42)
  p <- 4
  n <- 10000
  A <- rnorm(n, mean = 0, sd = 1)
  B <- rnorm(n, mean = 0, sd = 1)
  C <- A + rnorm(n, mean = 0, sd = 1)
  D <- A + B + rnorm(n, mean = 0, sd = 1)
  X <- data.frame(A = A, B = B, C = C, D = D)
  colnames(X) <- LETTERS[1:p]

  # fit
  score <- new("GaussL0penObsScore", X)
  ges_fit <- pcalg::ges(
    score = score,
    labels = colnames(X)
  )
  ess <- ges_fit$essgraph

  # discography
  disco_tbl <- tibble::as_tibble(discography(ess))
  # expected edge table
  edge_tbl <- tibble::tibble(
    from      = c("A", "A", "B"),
    to        = c("C", "D", "D"),
    edge_type = c("---", "-->", "-->")
  )
  testthat::expect_equal(disco_tbl, edge_tbl)
})


# ──────────────────────────────────────────────────────────────────────────────
# fciAlgo, amat, amat.pag, amat.cpdag
# ──────────────────────────────────────────────────────────────────────────────

testthat::test_that("conversion from empty amat.pag works", {
  nodes <- LETTERS[1:4]
  amat <- matrix(0, 4, 4, dimnames = list(nodes, nodes))
  class(amat) <- c("amat.pag", "matrix")

  edge_tbl <- tibble::tibble(from = character(), to = character(), edge_type = character())

  disco_tbl <- tibble::as_tibble(discography(amat))

  testthat::expect_equal(disco_tbl, edge_tbl)
})

testthat::test_that("conversion from amat.pag works", {
  # codes: 0 none | 1 circle | 2 arrowhead | 3 tail   (column-mark convention)
  nodes <- LETTERS[1:4]
  amat <- matrix(0, 4, 4, dimnames = list(nodes, nodes))
  amat[1, 2] <- 2
  amat[2, 1] <- 3 # A --> B
  amat[1, 3] <- 1
  amat[3, 1] <- 2 # A --o C
  amat[2, 3] <- 3
  amat[3, 2] <- 1 # B o-> C
  amat[2, 4] <- 1
  amat[4, 2] <- 2 # B o-> D
  amat[3, 4] <- 3
  amat[4, 3] <- 1 # C --o D
  class(amat) <- c("amat.pag", "matrix")

  pag_edge_tbl <- tibble::tibble(
    from      = c("A", "C", "C", "D", "D"),
    to        = c("B", "A", "B", "B", "C"),
    edge_type = c("-->", "o->", "--o", "o->", "--o")
  )

  disco_tbl <- tibble::as_tibble(discography(amat))

  testthat::expect_equal(disco_tbl, pag_edge_tbl)
})

testthat::test_that("conversion from amat.cpdag works", {
  nodes <- LETTERS[1:4]
  amat <- matrix(0, 4, 4, dimnames = list(nodes, nodes))

  # A --> B     (arrowhead in B’s row, A’s column)
  amat["B", "A"] <- 1

  # C --> B
  amat["B", "C"] <- 1

  # A --- D     (1 in both directions)
  amat["A", "D"] <- 1
  amat["D", "A"] <- 1

  # C --- D
  amat["C", "D"] <- 1
  amat["D", "C"] <- 1

  class(amat) <- c("amat.cpdag", "matrix")

  cpdag_edge_tbl <- tibble::tibble(
    from      = c("A", "A", "C", "C"),
    to        = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  )

  disco_tbl <- tibble::as_tibble(discography(amat))

  testthat::expect_equal(disco_tbl, cpdag_edge_tbl)
})


testthat::test_that("conversion from bare amat (default method chooses cpdag)", {
  nodes <- LETTERS[1:4]
  amat <- matrix(0, 4, 4, dimnames = list(nodes, nodes))

  # encode   A --> B   by putting 1 in B’s row, A’s column
  amat["B", "A"] <- 1

  # encode   C --> B
  amat["B", "C"] <- 1

  # encode   A --- D   (1 in both directions)
  amat["A", "D"] <- 1
  amat["D", "A"] <- 1

  # encode   C --- D
  amat["C", "D"] <- 1
  amat["D", "C"] <- 1

  class(amat) <- c("amat", "matrix") # bare amat

  cpdag_edge_tbl <- tibble::tibble(
    from      = c("A", "A", "C", "C"),
    to        = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  )

  disco_tbl <- tibble::as_tibble(discography(amat))

  testthat::expect_equal(disco_tbl, cpdag_edge_tbl)
})


testthat::test_that("conversion from fciAlgo to amat to discography works", {
  # reproducible PAG via pcalg::fci
  set.seed(123)
  n <- 500
  p <- 4
  Sigma <- matrix(0.4, p, p)
  diag(Sigma) <- 1
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- LETTERS[1:4]
  suffStat <- list(C = stats::cor(X), n = n)

  fci_fit <- pcalg::fci(
    suffStat  = suffStat,
    indepTest = pcalg::gaussCItest,
    alpha     = 0.01,
    labels    = colnames(X),
    verbose   = FALSE
  )

  # compare discography on the S4 object with discography on its amat
  disco_tbl <- tibble::as_tibble(discography(fci_fit))
  reference_tbl <- tibble::as_tibble(discography(methods::as(fci_fit, "amat")))

  testthat::expect_equal(disco_tbl, reference_tbl)
})

# ──────────────────────────────────────────────────────────────────────────────
# Misc
# ──────────────────────────────────────────────────────────────────────────────


test_that("as_tibble_edges() infers and alphabetically sorts nodes when nodes = NULL", {
  # two directed edges; B appears before A in the input on purpose
  out <- as_tibble_edges(
    from  = c("B", "A"),
    to    = c("C", "C"),
    type  = c("-->", "-->")
  )

  # node order should be inferred as A, B, C (alphabetical)
  expect_equal(out$from, c("A", "B"))
  expect_equal(out$to, c("C", "C"))
})

# ──────────────────────────────────────────────────────────────────────────────
# new_discography()
# ──────────────────────────────────────────────────────────────────────────────

test_that("new_discography() checks required columns and coerces types", {
  # error path: edge_type column missing
  bad <- tibble::tibble(from = "A", to = "B")
  expect_error(
    new_discography(bad),
    "contain .*from.*to.*edge_type",
    perl = TRUE
  )

  # success path: factors become character, columns reordered
  ok <- tibble::tibble(
    weight     = 1, # extra column
    edge_type  = factor("-->"),
    to         = factor("B"),
    from       = factor("A")
  )

  dg <- new_discography(ok)

  expect_s3_class(dg, "discography")
  expect_equal(names(dg)[1:3], c("from", "to", "edge_type"))
  expect_type(dg$from, "character")
  expect_type(dg$edge_type, "character")
  expect_equal(dg$weight, 1)
})

test_that("discography.amat.cpdag() assigns nodes from colnames() or V1…Vn", {
  m1 <- matrix(0, 2, 2)
  colnames(m1) <- c("X", "Y") # triggers the colnames() fallback
  m1[1, 2] <- m1[2, 1] <- 1 # X --- Y
  class(m1) <- c("amat.cpdag", "matrix")

  tbl1 <- tibble::as_tibble(discography(m1))
  expect_equal(tbl1$from, "X")
  expect_equal(tbl1$to, "Y")

  m2 <- matrix(0, 2, 2, dimnames = list(NULL, NULL))
  m2[1, 2] <- m2[2, 1] <- 1 # V1 --- V2
  class(m2) <- c("amat.cpdag", "matrix")

  tbl2 <- tibble::as_tibble(discography(m2))
  expect_equal(tbl2$from, "V1")
  expect_equal(tbl2$to, "V2")
})

test_that("discography.amat.pag() assigns nodes from colnames() or V1…Vn", {
  m1 <- matrix(0, 2, 2)
  colnames(m1) <- c("X", "Y") # triggers the colnames() fallback
  m1[1, 2] <- m1[2, 1] <- 1 # X --- Y
  class(m1) <- c("amat.pag", "matrix")

  tbl1 <- tibble::as_tibble(discography(m1))
  expect_equal(tbl1$from, "X")
  expect_equal(tbl1$to, "Y")

  m2 <- matrix(0, 2, 2, dimnames = list(NULL, NULL))
  m2[1, 2] <- m2[2, 1] <- 1 # V1 --- V2
  class(m2) <- c("amat.pag", "matrix")

  tbl2 <- tibble::as_tibble(discography(m2))
  expect_equal(tbl2$from, "V1")
  expect_equal(tbl2$to, "V2")
})

test_that("discography.amat.cpdag() early-returns for an empty adjacency matrix", {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  class(m) <- c("amat.cpdag", "matrix")

  tbl <- tibble::as_tibble(discography(m))

  expect_equal(nrow(tbl), 0) # early-return path
  expect_named(tbl, c("from", "to", "edge_type"))
})

test_that("discography.amat() delegates to .amat.pag when object inherits amat.pag", {
  m <- matrix(0, 2, 2, dimnames = list(c("X", "Y"), c("X", "Y")))
  class(m) <- c("amat.pag", "matrix") # inherits("amat.pag") is TRUE

  tbl_via_switch <- discography.amat(m)
  tbl_direct <- discography.amat.pag(m)

  expect_equal(tbl_via_switch, tbl_direct)
})

test_that("conversion from empty tamat (pdag) works", {
  nodes <- LETTERS[1:4]
  m <- matrix(0L, 4, 4, dimnames = list(nodes, nodes))
  tm <- tamat(m, order = c("t1", "t2"), type = "pdag")

  disco_tbl <- tibble::as_tibble(discography(tm))
  expect_equal(
    disco_tbl,
    tibble::tibble(
      from = character(),
      to = character(),
      edge_type = character()
    )
  )
})

test_that("conversion from fully connected tamat (pdag) works", {
  nodes <- LETTERS[1:6]
  m <- matrix(0L, 6, 6, dimnames = list(nodes, nodes))

  # Encode all i -> j for i < j using CPDAG 0/1 convention:
  # A --> B  == put 1 in [B, A]
  pairs_from <- c(
    "A", "A", "A", "A", "A",
    "B", "B", "B", "B",
    "C", "C", "C",
    "D", "D",
    "E"
  )
  pairs_to <- c(
    "B", "C", "D", "E", "F",
    "C", "D", "E", "F",
    "D", "E", "F",
    "E", "F",
    "F"
  )
  for (k in seq_along(pairs_from)) {
    m[pairs_to[k], pairs_from[k]] <- 1L
  }

  tm <- tamat(m, order = c("t1", "t2"), type = "pdag")
  disco_tbl <- tibble::as_tibble(discography(tm))

  expect_equal(
    disco_tbl,
    tibble::tibble(
      from = pairs_from,
      to = pairs_to,
      edge_type = rep("-->", sum(1:5))
    )
  )
})

test_that("conversion from tamat cpdag mixture (directed + undirected) works", {
  nodes <- LETTERS[1:4]
  m <- matrix(0L, 4, 4, dimnames = list(nodes, nodes))

  # A --> B  (mark 1 at [B, A])
  m["B", "A"] <- 1L
  # C --> B
  m["B", "C"] <- 1L
  # A --- D  (1 both ways)
  m["A", "D"] <- 1L
  m["D", "A"] <- 1L
  # C --- D
  m["C", "D"] <- 1L
  m["D", "C"] <- 1L

  tm <- tamat(m, order = c("t1", "t2"), type = "pdag")
  disco_tbl <- tibble::as_tibble(discography(tm))

  cpdag_edge_tbl <- tibble::tibble(
    from      = c("A", "A", "C", "C"),
    to        = c("B", "D", "B", "D"),
    edge_type = c("-->", "---", "-->", "---")
  )
  expect_equal(disco_tbl, cpdag_edge_tbl)
})

test_that("tamat defaults to pdag when type is NULL and no prior attr", {
  nodes <- c("X", "Y")
  m <- matrix(0L, 2, 2, dimnames = list(nodes, nodes))
  # X --- Y
  m["X", "Y"] <- 1L
  m["Y", "X"] <- 1L

  tm <- tamat(m, order = "t", type = NULL) # no attr on input
  disco_tbl <- tibble::as_tibble(discography(tm))

  expect_equal(disco_tbl, tibble::tibble(from = "X", to = "Y", edge_type = "---"))
})

test_that("tamat respects existing tamat_type attr when type = NULL", {
  nodes <- c("A", "B")
  m <- matrix(0L, 2, 2, dimnames = list(nodes, nodes))
  # A o-o B (circles both ends in PAG encoding)
  m["A", "B"] <- 1L
  m["B", "A"] <- 1L
  attr(m, "tamat_type") <- "ag"

  tm <- tamat(m, order = "t", type = NULL) # should keep 'ag'
  disco_tbl <- tibble::as_tibble(discography(tm))

  expect_equal(disco_tbl, tibble::tibble(from = "A", to = "B", edge_type = "o-o"))
})

# ──────────────────────────────────────────────────────────────────────────────
# tamat
# ──────────────────────────────────────────────────────────────────────────────

test_that("conversion from tamat PAG/AG works (mixed marks)", {
  # codes: 0 none | 1 circle | 2 arrow | 3 tail   (column-oriented)
  nodes <- LETTERS[1:4]
  m <- matrix(0L, 4, 4, dimnames = list(nodes, nodes))
  # A --> B     : [B,A]=2? (No: PAG uses 3 tail & 2 arrow; see below)
  # Use same pattern as your amat.pag test:
  m[1, 2] <- 2
  m[2, 1] <- 3 # A --> B
  m[1, 3] <- 1
  m[3, 1] <- 2 # A --o C
  m[2, 3] <- 3
  m[3, 2] <- 1 # B o-> C
  m[2, 4] <- 1
  m[4, 2] <- 2 # B o-> D
  m[3, 4] <- 3
  m[4, 3] <- 1 # C --o D

  tm <- tamat(m, order = c("t1", "t2"), type = "ag")
  disco_tbl <- tibble::as_tibble(discography(tm))

  pag_edge_tbl <- tibble::tibble(
    from      = c("A", "C", "C", "D", "D"),
    to        = c("B", "A", "B", "B", "C"),
    edge_type = c("-->", "o->", "--o", "o->", "--o")
  )
  expect_equal(disco_tbl, pag_edge_tbl)
})

test_that("tamat accepts 'pag' alias for AG/PAG handling", {
  nodes <- c("a", "b")
  m <- matrix(0L, 2, 2, dimnames = list(nodes, nodes))
  # a o-o b
  m["a", "b"] <- 1L
  m["b", "a"] <- 1L

  tm <- tamat(m, order = "t", type = "pag")
  disco_tbl <- tibble::as_tibble(discography(tm))

  expect_equal(disco_tbl, tibble::tibble(from = "a", to = "b", edge_type = "o-o"))
})

test_that("discography.tamat falls back to V1…Vn when dimnames are missing", {
  m <- matrix(0L, 2, 2) # no names
  m[1, 2] <- 1L
  m[2, 1] <- 1L # V1 --- V2 (pdag)
  tm <- tamat(m, order = "t", type = "pdag")

  disco_tbl <- tibble::as_tibble(discography(tm))
  expect_equal(disco_tbl, tibble::tibble(from = "V1", to = "V2", edge_type = "---"))
})

test_that("discography.tamat errors on unknown tamat_type", {
  m <- matrix(0L, 1, 1)
  tm <- tamat(m, order = "t", type = "weird")

  expect_error(discography(tm), "Unknown `tamat_type`")
})
