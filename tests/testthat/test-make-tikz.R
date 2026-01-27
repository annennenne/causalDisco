test_that("make_tikz produces correct snippet for simple A --> B graph", {
  cg <- caugi::caugi(A %-->% B)
  layout <- data.frame(
    name = c("A", "B"),
    x = c(0, 0),
    y = c(0, 1)
  )
  plot_obj <- caugi::plot(cg, layout = layout)

  tikz_snippet <- make_tikz(plot_obj, full_doc = FALSE)

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

  tikz_snippet_bend <- make_tikz(plot_obj, full_doc = FALSE, bend_edges = TRUE)
  expect_true(grepl(
    "(A) edge[bend left=25, -Latex] (B)",
    tikz_snippet_bend,
    fixed = TRUE
  ))

  tikz_snippet_full <- make_tikz(plot_obj, full_doc = TRUE)
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

  plot_obj <- caugi::plot(cg, layout = layout)

  tikz_snippet <- make_tikz(plot_obj, full_doc = FALSE)

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

  plot_obj <- caugi::plot(cg, layout = layout)

  tikz_snippet <- make_tikz(plot_obj, full_doc = FALSE)

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

  plot_obj <- caugi::plot(cg, layout = layout)

  tikz_snippet <- make_tikz(plot_obj, full_doc = FALSE, bend_angle = 10)

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
