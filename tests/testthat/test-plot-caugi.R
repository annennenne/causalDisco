test_that("Plotting caugi objects work", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A", "B", "C", "D", "E")) |> # A, B, C, D, E
    caugi::add_edges(A %-->% B %-->% C) |> # A --> B --> C, D, E
    caugi::set_edges(B %---% C) # A --> B --- C, D, E

  plot(cg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi with empty knowledge works", {
  kn <- knowledge()
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A1", "A2", "B1", "B2", "C1")) |>
    caugi::add_edges(A1 %-->% B1 %-->% C1) |>
    caugi::set_edges(B2 %---% C1)
  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("Plotting knowledge objects with required+forbidden works", {
  data(tpc_example)
  kn <- knowledge(
    tpc_example,
    child_x1 %-->% youth_x3,
    child_x2 %!-->% oldage_x5
  )
  plot(kn)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi and knowledge objects work", {
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
  plot(kn)

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi and knowledge objects with only some of variables in tiers works", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A", "B", "C")) |>
    caugi::add_edges(A %-->% B) |>
    caugi::set_edges(B %---% C)

  kn <- knowledge(
    data.frame(A = 1, B = 2, C = 3),
    tier(
      first ~ A,
      second ~ B
    )
  )

  expect_warning(
    plot(kn),
    "Not all nodes are assigned to tiers."
  )

  kcg <- knowledgeable_caugi(cg, kn)
  expect_warning(
    plot(kcg),
    "Not all nodes are assigned to tiers."
  )
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi and knowledge objects with tier+required", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A1", "A2", "B1", "B2", "C1")) |>
    caugi::add_edges(A1 + A2 %-->% B1 %-->% C1) |>
    caugi::set_edges(B2 %---% C1)

  kn <- knowledge(
    tier(
      child ~ A1 + A2,
      youth ~ B1 + B2,
      old ~ C1
    ),
    A2 %-->% B1
  )
  plot(kn)

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi and knowledge objects with tier+forbidden", {
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
    A2 %!-->% B1,
    B1 %!-->% A2
  )
  plot(kn)

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi and knowledge objects with required", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A1", "A2", "B1", "B2", "C1")) |>
    caugi::add_edges(A1 %-->% B1 %-->% C1) |>
    caugi::set_edges(B2 %---% C1)

  kn <- knowledge(
    A1 %-->% B1
  )
  plot(kn)

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("Plotting knowledgeable_caugi and knowledge objects with forbidden", {
  cg <- caugi::caugi(class = "PDAG")

  cg <- cg |>
    caugi::add_nodes(c("A1", "A2", "B1", "B2", "C1")) |>
    caugi::add_edges(A1 %-->% B1 %-->% C1) |>
    caugi::set_edges(B2 %---% C1)

  kn <- knowledge(
    A2 %!-->% B1,
    B1 %!-->% A2
  )
  plot(kn)

  kcg <- knowledgeable_caugi(cg, kn)
  plot(kcg)
  expect_true(TRUE)
})

test_that("disco plotting works", {
  data(tpc_example)

  # define background knowledge object
  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  # use causalDisco's own tges algorithm with temporal BIC score
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)

  plot(disco_cd_tges)
  expect_true(TRUE)
})

test_that("disco plotting with required works", {
  data(tpc_example)

  # define background knowledge object
  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    ),
    child_x1 %-->% youth_x3
  )

  # use causalDisco's own tges algorithm with temporal BIC score
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)

  plot(disco_cd_tges)
  expect_true(TRUE)
})

test_that("disco plotting with forbidden works", {
  data(tpc_example)

  # define background knowledge object
  kn <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    ),
    child_x1 %!-->% youth_x3
  )

  # use causalDisco's own tges algorithm with temporal BIC score
  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)

  plot(disco_cd_tges)
  expect_true(TRUE)
})
