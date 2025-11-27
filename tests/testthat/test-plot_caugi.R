test_that("Plotting caugi objects work", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A", "B", "C", "D", "E")) |> # A, B, C, D, E
    caugi::add_edges(A %-->% B %-->% C) |> # A --> B --> C, D, E
    caugi::set_edges(B %---% C) # A --> B --- C, D, E

  plot(cg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi objects work", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A1", "A2", "B1", "B2", "C1")) |>
    caugi::add_edges(A1 %-->% B1 %-->% C1) |>
    caugi::set_edges(B2 %---% C1)

  kn <- knowledge(
    tier(
      child ~ A1 + A2,
      youth ~ B1 + B2,
      old ~ C1
    )
  )

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi objects with required", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A1", "A2", "B1", "B2", "C1")) |>
    caugi::add_edges(A1 %-->% B1 %-->% C1) |>
    caugi::set_edges(B2 %---% C1)

  kn <- knowledge(
    tier(
      child ~ A1 + A2,
      youth ~ B1 + B2,
      old ~ C1
    ),
    required(A2 ~ B1)
  )

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("disco plotting works", {
  data("tpcExample")

  # define background knowledge object
  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  # use causalDisco's own tges algorithm with temporal BIC score
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  plot(disco_cd_tges)
  expect_true(TRUE)
})

test_that("disco plotting with required works", {
  data("tpcExample")

  # define background knowledge object
  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    ),
    required(child_x1 ~ youth_x3)
  )

  # use causalDisco's own tges algorithm with temporal BIC score
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  plot(disco_cd_tges)
  expect_true(TRUE)
})

test_that("disco plotting with forbidden works", {
  data("tpcExample")

  # define background knowledge object
  kn <- knowledge(
    tpcExample,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    ),
    forbidden(child_x1 ~ youth_x3)
  )

  # use causalDisco's own tges algorithm with temporal BIC score
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(data = tpcExample, method = cd_tges, knowledge = kn)

  plot(disco_cd_tges)
  expect_true(TRUE)
})
