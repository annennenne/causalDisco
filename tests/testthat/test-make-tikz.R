test_that("make_tikz produces correct snippet for simple A --> B graph", {
  cg <- caugi::caugi(A %-->% B)
  layout <- data.frame(
    name = c("A", "B"),
    x = c(0, 0),
    y = c(0, 1)
  )
  tikz_snippet <- make_tikz(cg, layout = layout, full_doc = FALSE)

  # ---- Check TikZ basics ----
  expect_true(grepl(
    "\\tikzset{arrows={[scale=3]}",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl(
    "every node/.style={fill=lightgray",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl("\\begin{tikzpicture}", tikz_snippet, fixed = TRUE))
  expect_true(grepl("\\end{tikzpicture}", tikz_snippet, fixed = TRUE))

  # ---- Check nodes ----
  expect_true(grepl(
    "\\node[draw, circle] (A) at (0,0) {A};",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl(
    "\\node[draw, circle] (B) at (0,10) {B};",
    tikz_snippet,
    fixed = TRUE
  ))

  # ---- Check edge ----
  expect_true(grepl("(A) edge[, -Latex] (B)", tikz_snippet, fixed = TRUE))

  tikz_snippet_bend <- make_tikz(cg, full_doc = FALSE, bend_edges = TRUE)
  expect_true(grepl(
    "(A) edge[bend left=25, -Latex] (B)",
    tikz_snippet_bend,
    fixed = TRUE
  ))

  tikz_snippet_full <- make_tikz(cg, full_doc = TRUE)
  expect_true(grepl(
    "\\documentclass[tikz,border=2mm]{standalone}",
    tikz_snippet_full,
    fixed = TRUE
  ))
})

test_that("make_tikz produces correct snippet for simple A --- B graph", {
  cg <- caugi::caugi(A %---% B)
  layout <- data.frame(
    name = c("A", "B"),
    x = c(0, 0),
    y = c(1, 0)
  )

  tikz_snippet <- make_tikz(cg, layout = layout, full_doc = FALSE)

  # ---- Check TikZ basics ----
  expect_true(grepl(
    "every node/.style={fill=lightgray",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl("\\begin{tikzpicture}", tikz_snippet, fixed = TRUE))
  expect_true(grepl("\\end{tikzpicture}", tikz_snippet, fixed = TRUE))

  # ---- Check nodes ----
  expect_true(grepl(
    "\\node[draw, circle] (A) at (0,10) {A};",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl(
    "\\node[draw, circle] (B) at (0,0) {B};",
    tikz_snippet,
    fixed = TRUE
  ))

  # ---- Check edge ----
  expect_true(grepl("(A) edge[, -] (B)", tikz_snippet, fixed = TRUE))
})

test_that("make_tikz produces correct snippet for simple A <-> B graph", {
  cg <- caugi::caugi(A %<->% B)
  layout <- data.frame(
    name = c("A", "B"),
    x = c(1, 0),
    y = c(0, 0)
  )

  tikz_snippet <- make_tikz(cg, layout = layout, full_doc = FALSE)

  # ---- Check TikZ basics ----
  expect_true(grepl(
    "every node/.style={fill=lightgray",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl("\\begin{tikzpicture}", tikz_snippet, fixed = TRUE))
  expect_true(grepl("\\end{tikzpicture}", tikz_snippet, fixed = TRUE))

  # ---- Check nodes ----
  expect_true(grepl(
    "\\node[draw, circle] (A) at (10,0) {A};",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl(
    "\\node[draw, circle] (B) at (0,0) {B};",
    tikz_snippet,
    fixed = TRUE
  ))

  # ---- Check edge ----
  expect_true(grepl(
    "(A) edge[, {Latex}-{Latex}] (B)",
    tikz_snippet,
    fixed = TRUE
  ))
})

test_that("make_tikz produces bent edges automatically for A --> B, B --> A graph", {
  cg <- caugi::caugi(A %-->% B, B %-->% A, simple = FALSE, class = "UNKNOWN")
  layout <- data.frame(
    name = c("A", "B"),
    x = c(1, 0),
    y = c(0, 0)
  )

  tikz_snippet <- make_tikz(
    cg,
    layout = layout,
    full_doc = FALSE,
    bend_angle = 10
  )

  # ---- Check edge ----
  expect_true(grepl(
    "(A) edge[bend left=10, -Latex] (B)",
    tikz_snippet,
    fixed = TRUE
  ))
  expect_true(grepl(
    "(B) edge[bend left=10, -Latex] (A)",
    tikz_snippet,
    fixed = TRUE
  ))
})

test_that("make_tikz works on required and forbidden knowledge", {
  kn <- knowledge(
    A %-->% B,
    A %-->% C,
    B %!-->% C
  )

  tikz_snippet <- make_tikz(
    kn,
    required_col = "blue",
    forbidden_col = "green",
    full_doc = FALSE,
    bend_angle = 10
  )

  # Global color should be blue (2 required)
  expect_true(grepl(
    "style={draw=blue}",
    tikz_snippet,
    fixed = TRUE
  ))

  # The forbidden edge should be green
  expect_true(grepl(
    "(B) edge[draw=green, -Latex] (C)",
    tikz_snippet,
    fixed = TRUE
  ))
})

test_that("make_tikz works on tiered knowledge", {
  data(tpc_example)
  kn_tiered <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )

  tikz_snippet <- make_tikz(
    kn_tiered,
    full_doc = FALSE
  )

  expect_true(grepl(
    "node[draw, rectangle, fill=blue!20, rounded corners, inner sep=0.5cm, fit=(child_x1)(child_x2)] (child)",
    tikz_snippet,
    fixed = TRUE
  ))

  cd_tges <- tges(engine = "causalDisco", score = "tbic")
  disco_cd_tges <- disco(
    data = tpc_example,
    method = cd_tges,
    knowledge = kn_tiered
  )

  tikz_snippet <- make_tikz(
    disco_cd_tges,
    scale = 10,
    full_doc = FALSE
  )

  expect_true(grepl(
    "begin{scope}[on background layer]",
    tikz_snippet,
    fixed = TRUE
  ))

  expect_true(grepl(
    "node[draw, rectangle, fill=blue!20, rounded corners, inner sep=0.5cm, fit=(child_x1)(child_x2)] (child)",
    tikz_snippet,
    fixed = TRUE
  ))

  expect_true(grepl(
    "node[anchor=south, draw=none, fill=none] at ($(child.north)+(0cm,0.2cm)$) {child}",
    tikz_snippet,
    fixed = TRUE
  ))
})

test_that("make_tikz works with custom tier", {
  data(tpc_example)
  kn_tiered <- knowledge(
    tpc_example,
    tier(
      child ~ starts_with("child"),
      youth ~ starts_with("youth"),
      old ~ starts_with("old")
    )
  )
  tiers <- list(
    young = c("child_x1", "child_x2", "youth_x3", "youth_x4"),
    old = c("oldage_x5", "oldage_x6")
  )
  tikz_snippet <- make_tikz(
    kn_tiered,
    layout = "tiered",
    tiers = tiers,
    full_doc = FALSE
  )
  expect_true(grepl(
    "(young)",
    tikz_snippet,
    fixed = TRUE
  ))
})
