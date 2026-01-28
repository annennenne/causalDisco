########### rcolor_to_tikz Tests #########

test_that("rcolor_to_tikz handles NULL and empty input", {
  expect_equal(rcolor_to_tikz(NULL), list(tikz = NULL, r_col = NULL))
  expect_equal(rcolor_to_tikz(character(0)), list(tikz = NULL, r_col = NULL))
})

test_that("rcolor_to_tikz handles default TikZ colors", {
  defaults <- c(
    "black",
    "blue",
    "brown",
    "cyan",
    "darkgray",
    "gray",
    "green",
    "lightgray",
    "magenta",
    "orange",
    "pink",
    "purple",
    "red"
  )
  for (col in defaults) {
    expect_equal(rcolor_to_tikz(col), list(tikz = col, r_col = NULL))
    expect_equal(rcolor_to_tikz(toupper(col)), list(tikz = col, r_col = NULL))
  }
})

test_that("rcolor_to_tikz maps 'lightgrey' to 'lightgray'", {
  expect_equal(
    rcolor_to_tikz("lightgrey"),
    list(tikz = "lightgray", r_col = NULL)
  )
  expect_equal(
    rcolor_to_tikz("LIGHTGREY"),
    list(tikz = "lightgray", r_col = NULL)
  )
})

test_that("rcolor_to_tikz handles custom colors", {
  # Example: purple-ish color
  result <- rcolor_to_tikz("#123456")
  expect_equal(result$r_col, "#123456")
  expect_true(grepl("\\{rgb:red,.*;green,.*;blue,.*\\}", result$tikz))

  # Another example using R color name not in defaults
  result <- rcolor_to_tikz("goldenrod")
  expect_equal(result$r_col, "goldenrod")
  expect_true(grepl("\\{rgb:red,.*;green,.*;blue,.*\\}", result$tikz))
})

######### extract_nodes Tests #########

test_that("extract_nodes returns correct nodes with styles and labels", {
  # Mock grobs
  node_grob_children <- list(
    node.A = list(
      name = "node.A",
      x = 1,
      y = 2,
      gp = list(fill = "red", col = "black", fontsize = 12)
    ),
    node.B = list(
      name = "node.B",
      x = 3,
      y = 4,
      gp = list(fill = "blue", col = NULL, fontsize = 10)
    ),
    label.A = list(label = "Node A"),
    label.B = list(label = "Node B"),
    other = list(something = TRUE) # Should be ignored
  )

  scale <- 2
  result <- extract_nodes(node_grob_children, scale)

  # Check correct number of nodes
  expect_equal(length(result), 2)

  # Check names and labels
  expect_equal(result[[1]]$name, "Node A")
  expect_equal(result[[2]]$name, "Node B")
  expect_equal(result[[1]]$label, "Node A")
  expect_equal(result[[2]]$label, "Node B")

  # Check coordinates scaling
  expect_equal(result[[1]]$x, 2) # 1 * 2
  expect_equal(result[[1]]$y, 4) # 2 * 2
  expect_equal(result[[2]]$x, 6) # 3 * 2
  expect_equal(result[[2]]$y, 8) # 4 * 2

  # Check styles
  expect_equal(result[[1]]$style$fill$tikz, "red")
  expect_equal(result[[1]]$style$draw$tikz, "black")
  expect_equal(result[[1]]$style$font, "\\fontsize{12}{14}\\selectfont")

  expect_equal(result[[2]]$style$fill$tikz, "blue")
  expect_null(result[[2]]$style$draw) # col=NULL
  expect_equal(result[[2]]$style$font, "\\fontsize{10}{12}\\selectfont")
})

test_that("extract_nodes handles empty input gracefully", {
  empty_result <- extract_nodes(list(), 1)
  expect_equal(empty_result, list())
})

test_that("extract_nodes works when gp elements are missing", {
  node_grob_children <- list(
    node.C = list(name = "C", x = 0, y = 0, gp = list()),
    label.C = list(label = "Node C")
  )
  result <- extract_nodes(node_grob_children, 1)
  expect_equal(result[[1]]$style, list()) # No styles applied
  expect_equal(result[[1]]$x, 0)
  expect_equal(result[[1]]$y, 0)
})


############ compute_global_color Tests #########

test_that("compute_global_color returns most common color", {
  nodes <- list(
    list(style = list(fill = list(tikz = "red"))),
    list(style = list(fill = list(tikz = "blue"))),
    list(style = list(fill = list(tikz = "red"))),
    list(style = list(fill = list(tikz = "green")))
  )

  expect_equal(compute_global_color(nodes, "fill"), "red")
})

test_that("compute_global_color ignores nodes with missing style or tikz", {
  nodes <- list(
    list(style = list(fill = list(tikz = "red"))),
    list(style = list(fill = NULL)), # missing fill
    list(style = list(draw = list(tikz = "blue"))), # no fill
    list(style = list(fill = list(tikz = "red")))
  )

  expect_equal(compute_global_color(nodes, "fill"), "red")
})

test_that("compute_global_color returns NULL for empty input or no colors", {
  expect_null(compute_global_color(list(), "fill"))

  nodes <- list(
    list(style = list(draw = list(tikz = "blue"))),
    list(style = list(draw = list(tikz = "green")))
  )
  expect_null(compute_global_color(nodes, "fill"))
})


############# latex_escape Tests #########

test_that("latex_escape escapes all LaTeX special characters", {
  specials <- c("_", "%", "#", "&", "$", "^", "{", "}", "~")

  for (ch in specials) {
    result <- latex_escape(ch)
    expect_equal(result, paste0("\\", ch))
  }
})

test_that("latex_escape escapes backslash correctly", {
  expect_equal(latex_escape("\\"), "\\textbackslash\\{\\}")
})

test_that("latex_escape handles strings with multiple specials", {
  input <- "100% of $people & 50_50 #tests {ok} ~yes^no \\"
  expected <- "100\\% of \\$people \\& 50\\_50 \\#tests \\{ok\\} \\~yes\\^no \\textbackslash\\{\\}"
  expect_equal(latex_escape(input), expected)
})

test_that("latex_escape leaves normal text unchanged", {
  expect_equal(latex_escape("Hello World!"), "Hello World!")
})

test_that("latex_escape handles empty string", {
  expect_equal(latex_escape(""), "")
})


######### build_node_lines Tests #########

test_that("build_node_lines adds style attributes correctly", {
  nodes <- list(
    list(
      name = "node1",
      x = 1,
      y = 2,
      label = "A",
      style = list(
        fill = list(tikz = "red"),
        draw = list(tikz = "black"),
        font = "\\fontsize{12}{14}\\selectfont"
      )
    ),
    list(
      name = "node2",
      x = 3,
      y = 4,
      label = "B",
      style = list(
        fill = list(tikz = "red") # same as global, should be skipped
      )
    )
  )

  global_fill <- "red"
  global_draw <- "black"

  lines <- build_node_lines(nodes, global_fill, global_draw)

  # Node 1: fill and draw match global, so style_list should include only font
  expect_true(grepl("font=", lines[1]))
  expect_false(grepl("fill=red", lines[1]))
  expect_false(grepl("draw=black", lines[1]))
  expect_true(grepl("\\(node1\\) at \\(1,2\\) \\{A\\};", lines[1]))

  # Node 2: fill same as global, no draw, no other style
  expect_false(grepl("fill=", lines[2]))
  expect_false(grepl("draw=", lines[2]))
})

test_that("build_node_lines handles custom styles besides fill/draw", {
  nodes <- list(
    list(
      name = "nodeY",
      x = 1,
      y = 1,
      label = "L",
      style = list(
        font = "\\fontsize{10}{12}\\selectfont"
      )
    )
  )
  lines <- build_node_lines(
    nodes,
    global_node_fill = NULL,
    global_node_draw = NULL
  )
  expect_true(grepl("font=", lines[1]))
})

test_that("build_node_lines adds non-global fill/draw correctly", {
  nodes <- list(
    list(
      name = "node1",
      x = 0,
      y = 0,
      label = "A",
      style = list(
        fill = list(tikz = "blue"), # different from global
        draw = list(tikz = "green") # different from global
      )
    )
  )

  global_fill <- "red"
  global_draw <- "black"

  lines <- build_node_lines(nodes, global_fill, global_draw)

  # Both fill and draw differ from globals, so they should appear in style_list
  expect_true(grepl("fill=blue", lines[1]))
  expect_true(grepl("draw=green", lines[1]))
  expect_true(grepl("\\(node3\\) at \\(0,0\\) \\{C\\};", lines[1]))
})


######### get_anchor_and_offset Tests #########

test_that("get_anchor_and_offset returns correct anchors for above", {
  res <- get_anchor_and_offset("above")
  expect_equal(res$anchor, "south")
  expect_equal(res$anchor_point, "north")
  expect_equal(res$dx, 0)
  expect_equal(res$dy, 0.2) # default offset
})

test_that("get_anchor_and_offset returns correct anchors for below", {
  res <- get_anchor_and_offset("below")
  expect_equal(res$anchor, "north")
  expect_equal(res$anchor_point, "south")
})

test_that("get_anchor_and_offset returns correct anchors for left", {
  res <- get_anchor_and_offset("left")
  expect_equal(res$anchor, "east")
  expect_equal(res$anchor_point, "west")
})

test_that("get_anchor_and_offset returns correct anchors for right", {
  res <- get_anchor_and_offset("right")
  expect_equal(res$anchor, "west")
  expect_equal(res$anchor_point, "east")
})

test_that("get_anchor_and_offset respects custom offset", {
  res <- get_anchor_and_offset("above", offset = 0.5)
  expect_equal(res$dy, 0.5)
  res <- get_anchor_and_offset("left", offset = 1)
  expect_equal(res$dx, -1)
})

test_that("get_anchor_and_offset errors for invalid position", {
  expect_error(
    get_anchor_and_offset("diagonal"),
    "tier_label_pos must be one of: above, below, left, right"
  )
})


############ build_tikz_globals Tests #########

test_that("build_tikz_globals returns empty string if no globals", {
  expect_equal(build_tikz_globals(NULL, NULL, NULL), "")
})

test_that("build_tikz_globals sets only node fill", {
  res <- build_tikz_globals("red", NULL, NULL)
  expect_equal(res, "\\tikzset{every node/.style={fill=red}}")
})

test_that("build_tikz_globals sets only node draw", {
  res <- build_tikz_globals(NULL, "blue", NULL)
  expect_equal(res, "\\tikzset{every node/.style={draw=blue}}")
})

test_that("build_tikz_globals sets only edge color", {
  res <- build_tikz_globals(NULL, NULL, "green")
  expect_equal(res, "\\tikzset{every path/.style={draw=green}}")
})

test_that("build_tikz_globals combines node fill and draw correctly", {
  res <- build_tikz_globals("red", "blue", NULL)
  # draw should append to fill inside the same node style
  expect_equal(res, "\\tikzset{every node/.style={fill=red, draw=blue}}")
})

test_that("build_tikz_globals combines node and edge settings", {
  res <- build_tikz_globals("red", "blue", "green")
  expect_equal(
    res,
    "\\tikzset{every node/.style={fill=red, draw=blue}, every path/.style={draw=green}}"
  )
})

test_that("build_tikz_globals combines draw with edge when fill is NULL", {
  res <- build_tikz_globals(NULL, "blue", "green")
  expect_equal(
    res,
    "\\tikzset{every node/.style={draw=blue}, every path/.style={draw=green}}"
  )
})


########### extract_edges Tests #########

test_that("extract_edges computes global arrow length correctly", {
  nodes <- list(
    list(label = "A", x = 0, y = 0),
    list(label = "B", x = 1, y = 0)
  )

  edges <- list(
    list(
      x0 = 0,
      y0 = 0,
      x1 = 1,
      y1 = 0,
      gp = list(),
      arrow = list(length = 0.5),
      edge_type = "-->"
    ),
    list(
      x0 = 0,
      y0 = 0,
      x1 = 1,
      y1 = 0,
      gp = list(),
      arrow = list(length = 0.5),
      edge_type = "-->"
    ),
    list(
      x0 = 0,
      y0 = 0,
      x1 = 1,
      y1 = 0,
      gp = list(),
      arrow = list(length = 1),
      edge_type = "-->"
    )
  )

  result <- extract_edges(
    edges,
    nodes,
    scale = 1,
    bend_edges = FALSE,
    bend_angle = 15,
    global_edge_color = NULL
  )

  expect_equal(result$global_arrow_length, 0.5) # most common
})

test_that("extract_edges generates correct edge line with styles", {
  nodes <- list(
    list(label = "A", x = 0, y = 0),
    list(label = "B", x = 1, y = 0)
  )

  edges <- list(
    list(
      x0 = 0,
      y0 = 0,
      x1 = 1,
      y1 = 0,
      gp = list(col = "red", lwd = 1),
      arrow = list(length = 0.8),
      edge_type = "-->"
    )
  )

  res <- extract_edges(
    edges,
    nodes,
    scale = 1,
    bend_edges = TRUE,
    bend_angle = 10,
    global_edge_color = "blue"
  )
  line <- res$edge_lines[1]

  # Should contain from/to labels
  expect_true(grepl("\\(A\\).*\\(B\\)", line))

  # Should contain draw=red and line width
  expect_true(grepl("draw=red", line))
  expect_true(grepl("line width=1", line))

  # Bend left/right added
  expect_true(grepl("bend left=10", line))

  # Arrow spec
  expect_true(grepl("-Latex", line))

  # Global arrow length
  expect_equal(res$global_arrow_length, 0.8)
})

test_that("extract_edges falls back for unknown edge type with warning", {
  nodes <- list(
    list(label = "A", x = 0, y = 0),
    list(label = "B", x = 1, y = 0)
  )
  edges <- list(list(
    x0 = 0,
    y0 = 0,
    x1 = 1,
    y1 = 0,
    gp = list(),
    arrow = list(),
    edge_type = "???"
  ))

  expect_warning(
    res <- extract_edges(
      edges,
      nodes,
      scale = 1,
      bend_edges = FALSE,
      bend_angle = 15,
      global_edge_color = NULL
    ),
    "Unknown edge type"
  )
  expect_true(grepl("-Latex", res$edge_lines[1]))
})

test_that("extract_edges handles empty edge list", {
  nodes <- list(list(label = "A", x = 0, y = 0))
  edges <- list()
  res <- extract_edges(
    edges,
    nodes,
    scale = 1,
    bend_edges = FALSE,
    bend_angle = 15,
    global_edge_color = NULL
  )
  expect_equal(res$edge_lines, list())
  expect_equal(res$global_arrow_length, 1)
})

test_that("extract_edges ignores missing gp or arrow attributes", {
  nodes <- list(
    list(label = "A", x = 0, y = 0),
    list(label = "B", x = 1, y = 0)
  )
  edges <- list(list(
    x0 = 0,
    y0 = 0,
    x1 = 1,
    y1 = 0,
    gp = list(),
    arrow = list(),
    edge_type = "-->"
  ))

  res <- extract_edges(
    edges,
    nodes,
    scale = 1,
    bend_edges = FALSE,
    bend_angle = 10,
    global_edge_color = NULL
  )
  line <- res$edge_lines[1]

  # Should not contain draw=, line width, or arrows=...
  expect_false(grepl("draw=", line))
  expect_false(grepl("line width=", line))
  expect_false(grepl("arrows=\\{\\[scale=", line))
})


########### format_coord Tests #########

test_that("format_coord formats coordinates correctly", {
  expect_equal(format_coord(1), "1")
  expect_equal(format_coord(1.1), "1.1")
  expect_equal(format_coord(1.11), "1.11")
  expect_equal(format_coord(1.111), "1.111")
  expect_equal(format_coord(1.1111), "1.111")
})
