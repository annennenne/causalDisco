test_that("new_knowledgeable_caugi stops if not knowledge", {
  kn <- 1
  cg <- caugi::as_caugi(
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0
      ),
      nrow = 4,
      byrow = TRUE
    ),
    class = "DAG"
  )

  expect_error(
    new_knowledgeable_caugi(cg, kn),
    "Input must be a knowledge instance."
  )
})

test_that("knowledgeable_caugi stops if not knowledge", {
  kn <- 1
  cg <- caugi::as_caugi(
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0
      ),
      nrow = 4,
      byrow = TRUE
    ),
    class = "DAG"
  )

  expect_error(
    knowledgeable_caugi.default(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    knowledgeable_caugi.pcAlgo(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    knowledgeable_caugi.fciAlgo(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    knowledgeable_caugi.tetrad_graph(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    knowledgeable_caugi.EssGraph(cg, kn),
    "Input must be a knowledge instance."
  )
})

test_that("knowledge helpers works", {
  expect_error(
    set_knowledge.knowledgeable_caugi(1, 1),
    "Input must be a knowledge instance."
  )

  kn <- knowledge()
  cg <- caugi::as_caugi(
    matrix(
      c(
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0
      ),
      nrow = 4,
      byrow = TRUE
    ),
    class = "DAG"
  )
  kcg <- knowledgeable_caugi(cg, kn)

  nodes_kcg <- nodes(kcg)
  expected_nodes <- tibble::tibble(
    name = c("V1", "V2", "V3", "V4")
  )
  expect_equal(nodes_kcg, expected_nodes)

  edges_kcg <- edges(kcg)
  expected_edges <- tibble::tibble(
    from = c("V1", "V2", "V3"),
    edge = c("-->", "-->", "-->"),
    to = c("V2", "V3", "V4")
  )
  expect_equal(edges_kcg, expected_edges)

  kcg_knowledge <- knowledge.knowledgeable_caugi(kcg)
  expect_equal(kcg_knowledge, kn)

  expect_null(`$.knowledgeable_caugi`(kcg, "hi"))
  output <- `$.knowledgeable_caugi`(kcg, "vars")
  expect_equal(nrow(output), 0)
  expect_equal(class(output)[1], "tbl_df")

  output <- `$<-.knowledgeable_caugi`(kcg, "hi", 1)
  expect_equal(output$hi, 1)

  output <- `$<-.knowledgeable_caugi`(kcg, "vars", 1)
  expect_equal(output$knowledge$vars, 1)

  output <- `[[.knowledgeable_caugi`(kcg, "hi")
  expect_null(output)

  output <- `[[.knowledgeable_caugi`(kcg, "vars")
  expect_equal(nrow(output), 0)
  expect_equal(class(output)[1], "tbl_df")

  output <- `[[.knowledgeable_caugi`(kcg, "knowledge")
  expect_true(is_knowledge(output))
  expect_false(is_knowledgeable_caugi(output))

  output <- `[[<-.knowledgeable_caugi`(kcg, "hi", 1)
  expect_equal(output$hi, 1)
  expect_equal(class(output)[1], "knowledgeable_caugi")
  expect_equal(class(output)[2], "knowledge")

  output <- `[[<-.knowledgeable_caugi`(kcg, "vars", 1)
  expect_equal(output$knowledge$vars, 1)
  expect_equal(class(output)[1], "knowledgeable_caugi")
  expect_equal(class(output)[2], "knowledge")
})
