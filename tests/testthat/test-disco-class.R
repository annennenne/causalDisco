test_that("new_disco stops if not knowledge", {
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
    new_disco(cg, kn),
    "Input must be a knowledge instance."
  )
})

test_that("as_disco stops if not knowledge", {
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
    as_disco.default(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    as_disco.pcAlgo(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    as_disco.fciAlgo(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    as_disco.tetrad_graph(cg, kn),
    "Input must be a knowledge instance."
  )
  expect_error(
    as_disco.EssGraph(cg, kn),
    "Input must be a knowledge instance."
  )
})

test_that("knowledge helpers works", {
  expect_error(
    set_knowledge.disco(1, 1),
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
  kcg <- as_disco(cg, kn)

  nodes_kcg <- nodes(kcg$caugi)
  expected_nodes <- tibble::tibble(
    name = c("V1", "V2", "V3", "V4")
  )
  expect_equal(nodes_kcg, expected_nodes)

  edges_kcg <- edges(kcg$caugi)
  expected_edges <- tibble::tibble(
    from = c("V1", "V2", "V3"),
    edge = c("-->", "-->", "-->"),
    to = c("V2", "V3", "V4")
  )
  expect_equal(edges_kcg, expected_edges)

  kcg_knowledge <- knowledge.disco(kcg)
  expect_equal(kcg_knowledge, kn)

  expect_null(`$.disco`(kcg, "hi"))
  output <- `$.disco`(kcg, "vars")
  expect_equal(nrow(output), 0)
  expect_equal(class(output)[1], "tbl_df")

  output <- `$<-.disco`(kcg, "hi", 1)
  expect_equal(output$hi, 1)

  output <- `$<-.disco`(kcg, "vars", 1)
  expect_equal(output$knowledge$vars, 1)

  output <- `[[.disco`(kcg, "hi")
  expect_null(output)

  output <- `[[.disco`(kcg, "vars")
  expect_equal(nrow(output), 0)
  expect_equal(class(output)[1], "tbl_df")

  output <- `[[.disco`(kcg, "knowledge")
  expect_true(is_knowledge(output))
  expect_false(is_disco(output))

  output <- `[[<-.disco`(kcg, "hi", 1)
  expect_equal(output$hi, 1)
  expect_equal(class(output), "disco")

  output <- `[[<-.disco`(kcg, "vars", 1)
  expect_equal(output$knowledge$vars, 1)
  expect_equal(class(output), "disco")
})


test_that("as_disco print and summary methods", {
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
  kcg <- as_disco(cg, kn)
  print(kcg)
  print(kcg, wide = TRUE)
  print(kcg, compact = TRUE)
  print(kcg, wide = TRUE, compact = TRUE)
  summary(kcg)
  expect_true(TRUE)
})

test_that("as_disco print and summary methods works for empty kcg", {
  kn <- knowledge()
  cg <- caugi::caugi()
  kcg <- as_disco(cg, kn)
  print(kcg)
  print(kcg, wide = TRUE)
  print(kcg, compact = TRUE)
  print(kcg, wide = TRUE, compact = TRUE)
  summary(kcg)
  expect_true(TRUE)
})
