# TODO make tiered plots

#' @title Generate TikZ Code from a Caugi Plot
#'
#' @description
#' Generates LaTeX TikZ code from a `caugi::caugi_plot` object, preserving
#' node positions, labels, and visual styles. Edges are rendered with arrows,
#' line widths, and colors. The output is readable LaTeX code that can be
#' directly compiled or modified.
#'
#' @param caugi_plot_obj A `caugi::caugi_plot` object.
#' @param scale Numeric scalar. Scaling factor for node coordinates. Default is `10`.
#' @param full_doc Logical. If `TRUE` (default), generates a full standalone
#'   LaTeX document. If `FALSE`, returns only the `tikzpicture` environment.
#' @param bend_edges Logical. If `TRUE`, edges are drawn with bent arrows to
#'   reduce overlap. Default is `FALSE`. Arrows bend left or right depending
#'   on the relative positions of nodes.
#' @param bend_angle Numeric scalar. Angle in degrees for bending arrows when
#'   `bend_edges = TRUE`. Default is `25`.
#'
#' @return A character string containing LaTeX TikZ code. Depending on
#'   `full_doc`, this is either:
#'   * a complete LaTeX document (`full_doc = TRUE`), or
#'   * only the `tikzpicture` environment (`full_doc = FALSE`).
#'
#' @details
#' The function traverses the plot object's grob structure to extract nodes and
#' edges. Supported features include:
#'
#' * **Nodes**
#'   - Fill color and draw color (supports both named colors and custom RGB values)
#'   - Font size
#'   - Coordinates are scaled by the `scale` parameter
#'
#' * **Edges**
#'   - Line color and width
#'   - Arrow scale
#'   - Optional bending to reduce overlapping arrows
#'
#' To make the generated TikZ code more concise and readable, the code uses
#' global style settings, and only specifies individual styles for nodes or edges
#' that differ from these global defaults.
#'
#' @examples
#'
#' cg <- caugi::caugi(A %-->% B + C)
#' plot_obj <- caugi::plot(cg, node_style = list(fill = "red"))
#'
#' # Full standalone document
#' tikz_code <- make_tikz(plot_obj, scale = 10, full_doc = TRUE)
#' cat(tikz_code)
#'
#' # Only the tikzpicture environment
#' tikz_snippet <- make_tikz(plot_obj, scale = 10, full_doc = FALSE)
#' cat(tikz_snippet)
#'
#' # With bent edges
#' tikz_bent <- make_tikz(plot_obj, scale = 10, full_doc = FALSE, bend_edges = TRUE)
#' cat(tikz_bent)
#'
#' @export
make_tikz <- function(
  caugi_plot_obj,
  scale = 10,
  full_doc = TRUE,
  bend_edges = FALSE,
  bend_angle = 25
) {
  stopifnot(inherits(caugi_plot_obj, "caugi::caugi_plot"))

  g <- caugi_plot_obj@grob$children$caugi.graph

  # ---------------- Helper Functions ---------------- #

  rcolor_to_tikz <- function(r_col) {
    if (is.null(r_col) || length(r_col) == 0) {
      return(list(tikz = NULL, r_col = NULL))
    }
    r_col <- tolower(r_col)
    if (r_col == "lightgrey") {
      r_col <- "lightgray"
    }

    tikz_defaults <- c(
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

    if (r_col %in% tikz_defaults) {
      return(list(tikz = r_col, r_col = NULL))
    }

    rgb_vals <- grDevices::col2rgb(r_col) / 255
    tikz_str <- sprintf(
      "{rgb:red,%.3f;green,%.3f;blue,%.3f}",
      rgb_vals[1],
      rgb_vals[2],
      rgb_vals[3]
    )
    list(tikz = tikz_str, r_col = r_col)
  }

  extract_nodes <- function(node_grob_children, scale) {
    circle_nodes <- node_grob_children[grepl(
      "^node\\.",
      names(node_grob_children)
    )]
    text_grobs <- node_grob_children[grepl(
      "^label\\.",
      names(node_grob_children)
    )]
    label_map <- stats::setNames(
      lapply(text_grobs, function(x) x$label),
      gsub("label\\.", "node.", names(text_grobs))
    )

    nodes <- lapply(circle_nodes, function(node) {
      gp <- node$gp
      style <- list()
      if (!is.null(gp$fill) && length(gp$fill) > 0) {
        style$fill <- rcolor_to_tikz(gp$fill)
      }
      if (!is.null(gp$col) && length(gp$col) > 0) {
        style$draw <- rcolor_to_tikz(gp$col)
      }
      if (!is.null(gp$fontsize) && length(gp$fontsize) > 0) {
        style$font <- sprintf(
          "\\fontsize{%.0f}{%.0f}\\selectfont",
          gp$fontsize,
          gp$fontsize * 1.2
        )
      }
      list(
        name = label_map[[node$name]],
        x = as.numeric(node$x) * scale,
        y = as.numeric(node$y) * scale,
        style = style,
        label = label_map[[node$name]]
      )
    })
    nodes
  }

  compute_global_color <- function(nodes, color_type) {
    vals <- sapply(nodes, function(n) {
      if (
        !is.null(n$style[[color_type]]) && !is.null(n$style[[color_type]]$tikz)
      ) {
        n$style[[color_type]]$tikz
      } else {
        NA
      }
    })
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      names(sort(table(vals), decreasing = TRUE))[1]
    } else {
      NULL
    }
  }

  extract_edges <- function(
    edge_grob_children,
    nodes,
    scale,
    bend_edges,
    bend_angle,
    global_edge_color
  ) {
    # ---- Determine global arrow scale (numeric, unit-safe) ----
    edge_lengths <- sapply(edge_grob_children, function(e) {
      if (!is.null(e$arrow$length) && length(e$arrow$length) > 0) {
        as.numeric(e$arrow$length)
      } else {
        NA_real_
      }
    })

    edge_lengths <- edge_lengths[!is.na(edge_lengths)]
    global_arrow_length <- if (length(edge_lengths) > 0) {
      as.numeric(names(sort(table(edge_lengths), decreasing = TRUE))[1])
    } else {
      1
    }

    # ---- Build edges ----
    edge_lines <- sapply(edge_grob_children, function(edge) {
      # Resolve from/to nodes
      from_node <- nodes[[which(sapply(nodes, function(n) {
        n$x == as.numeric(edge$x0) * scale &&
          n$y == as.numeric(edge$y0) * scale
      }))]]$label

      to_node <- nodes[[which(sapply(nodes, function(n) {
        n$x == as.numeric(edge$x1) * scale &&
          n$y == as.numeric(edge$y1) * scale
      }))]]$label

      style <- c()

      # ---- Color ----
      if (!is.null(edge$gp$col) && length(edge$gp$col) > 0) {
        col_tikz <- rcolor_to_tikz(edge$gp$col)
        if (!is.null(col_tikz$tikz) && col_tikz$tikz != global_edge_color) {
          style <- c(style, paste0("draw=", col_tikz$tikz))
        }
      }

      # ---- Line width ----
      if (!is.null(edge$gp$lwd) && length(edge$gp$lwd) > 0) {
        style <- c(style, paste0("line width=", edge$gp$lwd))
      }

      # ---- Arrow scale (unit-safe comparison) ----
      if (!is.null(edge$arrow$length) && length(edge$arrow$length) > 0) {
        edge_len_num <- as.numeric(edge$arrow$length)
        if (!is.na(edge_len_num) && edge_len_num != global_arrow_length) {
          style <- c(style, paste0("arrows={[scale=", edge_len_num, "]}"))
        }
      }

      # ---- Bend arrows ----
      if (bend_edges) {
        from_x <- nodes[[which(sapply(nodes, function(n) {
          n$label == from_node
        }))]]$x
        to_x <- nodes[[which(sapply(nodes, function(n) n$label == to_node))]]$x
        bend_dir <- if (to_x >= from_x) "bend left" else "bend right"
        style <- c(style, paste0(bend_dir, "=", bend_angle))
      }

      # ---- Edge type -> TikZ arrow spec ----
      arrow_spec <- switch(
        edge$edge_type,
        "-->" = "-Latex",
        "---" = "-",
        "<->" = "{Latex}-{Latex}",
        "-Latex" # Default fallback
      )

      style_str <- paste(style, collapse = ", ")
      sprintf(
        "(%s) edge[%s, %s] (%s)",
        from_node,
        style_str,
        arrow_spec,
        to_node
      )
    })

    list(
      edge_lines = edge_lines,
      global_arrow_length = global_arrow_length
    )
  }

  format_coord <- function(x) {
    if (x == round(x)) as.character(round(x)) else sprintf("%.3f", x)
  }

  build_node_lines <- function(nodes, global_node_fill, global_node_draw) {
    unlist(lapply(nodes, function(n) {
      style_list <- c("circle")
      for (sty_name in names(n$style)) {
        if (sty_name %in% c("fill", "draw")) {
          global_val <- if (sty_name == "fill") {
            global_node_fill
          } else {
            global_node_draw
          }
          if (
            !is.null(n$style[[sty_name]]) &&
              n$style[[sty_name]]$tikz != global_val
          ) {
            style_list <- c(
              style_list,
              paste0(sty_name, "=", n$style[[sty_name]]$tikz)
            )
          }
        } else {
          style_list <- c(
            style_list,
            paste0(sty_name, "=", n$style[[sty_name]])
          )
        }
      }
      if (!"draw" %in% names(n$style) && is.null(global_node_draw)) {
        style_list <- c("draw", style_list)
      }
      sprintf(
        "\\node[%s] (%s) at (%s,%s) {%s};",
        paste(style_list, collapse = ", "),
        n$name,
        format_coord(n$x),
        format_coord(n$y),
        n$label
      )
    }))
  }

  build_tikz_globals <- function(
    global_node_fill,
    global_node_draw,
    global_edge_color
  ) {
    tikz_global <- c()
    if (!is.null(global_node_fill)) {
      tikz_global <- c(
        tikz_global,
        paste0("every node/.style={fill=", global_node_fill, "}")
      )
    }
    if (!is.null(global_node_draw)) {
      if (length(tikz_global) > 0) {
        tikz_global[length(tikz_global)] <- sub(
          "\\}$",
          paste0(", draw=", global_node_draw, "}"),
          tikz_global[length(tikz_global)]
        )
      } else {
        tikz_global <- c(
          tikz_global,
          paste0("every node/.style={draw=", global_node_draw, "}")
        )
      }
    }
    if (!is.null(global_edge_color)) {
      tikz_global <- c(
        tikz_global,
        paste0("every path/.style={draw=", global_edge_color, "}")
      )
    }
    if (length(tikz_global) > 0) {
      paste0("\\tikzset{", paste(tikz_global, collapse = ", "), "}")
    } else {
      ""
    }
  }

  # ---------------- Main Logic ---------------- #

  node_grob_children <- g$children$node_gtree$children
  edge_grob_children <- g$children[grep("^edge", names(g$children))]

  nodes <- extract_nodes(node_grob_children, scale)
  global_node_fill <- compute_global_color(nodes, "fill")
  global_node_draw <- compute_global_color(nodes, "draw")

  edge_dummy_nodes <- lapply(edge_grob_children, function(e) {
    col <- if (!is.null(e$gp$col) && length(e$gp$col) > 0) {
      rcolor_to_tikz(e$gp$col)
    } else {
      list(tikz = NULL)
    }
    list(style = list(draw = col))
  })
  global_edge_color <- compute_global_color(edge_dummy_nodes, "draw")

  node_lines <- build_node_lines(nodes, global_node_fill, global_node_draw)
  edge_info <- extract_edges(
    edge_grob_children,
    nodes,
    scale,
    bend_edges,
    bend_angle,
    global_edge_color
  )
  edge_lines <- edge_info$edge_lines
  global_arrow_length <- edge_info$global_arrow_length

  tikz_global_line <- build_tikz_globals(
    global_node_fill,
    global_node_draw,
    global_edge_color
  )
  generator_line <- paste0(
    "%%% Generated by causalDisco (version ",
    utils::packageVersion("causalDisco"),
    ")"
  )

  tikz_code <- if (full_doc) {
    paste(
      generator_line,
      "\\documentclass{standalone}",
      "\\usepackage{tikz}",
      "\\usetikzlibrary{positioning, arrows.meta}",
      "",
      "\\begin{document}",
      sprintf("\\tikzset{arrows={[scale=%s]}}", global_arrow_length),
      tikz_global_line,
      "\\begin{tikzpicture}",
      paste(node_lines, collapse = "\n"),
      "",
      paste0("\\path ", paste(edge_lines, collapse = "\n      "), ";"),
      "",
      "\\end{tikzpicture}",
      "\\end{document}",
      sep = "\n"
    )
  } else {
    paste(
      generator_line,
      sprintf("\\tikzset{arrows={[scale=%s]}}", global_arrow_length),
      tikz_global_line,
      "\\begin{tikzpicture}",
      paste(node_lines, collapse = "\n"),
      "",
      paste0("\\path ", paste(edge_lines, collapse = "\n      "), ";"),
      "",
      "\\end{tikzpicture}",
      sep = "\n"
    )
  }

  tikz_code
}
