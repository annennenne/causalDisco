#' @title Generate TikZ Code from a Caugi Plot
#'
#' @description
#' Generates LaTeX TikZ code from a `caugi::caugi_plot` object, preserving
#' node positions, labels, and visual styles. Edges are rendered with arrows,
#' line widths, and colors. The output is readable LaTeX code that can be
#' directly compiled or modified.
#'
#' @param caugi_plot_obj A `caugi::caugi_plot` object.
#' @param tier_node_map Optional named list mapping tier names to vectors of
#'  node names. If provided, tier rectangles and labels are added to the TikZ
#'  output. Default is `NULL`.
#' @param scale Numeric scalar. Scaling factor for node coordinates. Default is `10`.
#' @param full_doc Logical. If `TRUE` (default), generates a full standalone
#'   LaTeX document. If `FALSE`, returns only the `tikzpicture` environment.
#' @param bend_edges Logical. If `TRUE`, edges are drawn with bent edges. Default is `FALSE`.
#'   Edges connecting the same pair of nodes in both directions (`A %-->% B` and `B %-->% A`)
#'   are automatically bent left and right to avoid overlap. Bend direction is automatically chosen
#'   to reduce overlap.
#' @param bend_angle Numeric scalar. Angle in degrees for bending arrows when
#'   `bend_edges = TRUE`. Default is `25`.
#' @param tier_label_pos Character string specifying the position of tier labels
#'   relative to the tier rectangles. Must be one of `"above"`, `"below"`, `"left"`, or `"right"`.
#'   Default is `"above"`.
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
#' The generated TikZ code uses global style settings, and edges are connected
#' to nodes by name (as opposed to hard-coded coordinates), making it easy to
#' modify the output further if needed.
#'
#' @example inst/roxygen-examples/make-tikz-example.R
#'
#' @export
make_tikz <- function(
  caugi_plot_obj,
  tier_node_map = NULL,
  scale = 10,
  full_doc = TRUE,
  bend_edges = FALSE,
  bend_angle = 25,
  tier_label_pos = c("above", "below", "left", "right")
) {
  tier_label_pos <- match.arg(tier_label_pos)
  if (is.null(tier_node_map)) {
    make_tikz_standard(
      caugi_plot_obj = caugi_plot_obj,
      scale = scale,
      full_doc = full_doc,
      bend_edges = bend_edges,
      bend_angle = bend_angle
    )
  } else {
    make_tikz_tiered(
      caugi_plot_obj = caugi_plot_obj,
      tier_node_map = tier_node_map,
      scale = scale,
      full_doc = full_doc,
      bend_edges = bend_edges,
      bend_angle = bend_angle,
      tier_label_pos = tier_label_pos
    )
  }
}

#' @title Core TikZ Generation Logic
#' @inheritParams make_tikz
#' @returns A character string containing LaTeX TikZ code.
#' @keywords internal
#' @noRd
.make_tikz_core <- function(
  caugi_plot_obj,
  scale = 10,
  tier_node_map = NULL,
  full_doc = TRUE,
  bend_edges = FALSE,
  bend_angle = 25,
  tier_label_pos = "above"
) {
  stopifnot(inherits(caugi_plot_obj, "caugi::caugi_plot"))

  g <- caugi_plot_obj@grob$children$caugi.graph
  node_grob_children <- g$children$node_gtree$children
  edge_grob_children <- g$children[grep("^edge", names(g$children))]

  # ---- Extract nodes ----
  nodes <- extract_nodes(node_grob_children, scale)

  # ---- Global node colors ----
  global_node_fill <- compute_global_color(nodes, "fill")
  global_node_draw <- compute_global_color(nodes, "draw")

  # ---- Global edge color ----
  edge_dummy_nodes <- lapply(edge_grob_children, function(e) {
    col <- if (!is.null(e$gp$col) && length(e$gp$col) > 0) {
      rcolor_to_tikz(e$gp$col)
    } else {
      list(tikz = NULL)
    }
    list(style = list(draw = col))
  })
  global_edge_color <- compute_global_color(edge_dummy_nodes, "draw")

  # ---- Node and edge lines ----
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

  # ---- Auto-bend bidirectional edges ----
  # Only if bend_edges = FALSE
  if (!bend_edges && length(edge_lines) > 1) {
    # Collect edge coordinates
    edge_coords <- lapply(edge_grob_children, function(e) {
      data.frame(
        x0 = e$x0,
        y0 = e$y0,
        x1 = e$x1,
        y1 = e$y1
      )
    })
    edge_coords <- do.call(rbind, edge_coords)

    # Find bidirectional edges: edges with reversed coordinates
    bidir_idx <- sapply(seq_len(nrow(edge_coords)), function(i) {
      any(
        edge_coords$x0[i] == edge_coords$x1[-i] &
          edge_coords$y0[i] == edge_coords$y1[-i] &
          edge_coords$x1[i] == edge_coords$x0[-i] &
          edge_coords$y1[i] == edge_coords$y0[-i]
      )
    })

    if (any(bidir_idx)) {
      # Apply bending to those edges
      edge_lines <- lapply(seq_along(edge_lines), function(i) {
        if (bidir_idx[i]) {
          gsub(
            "\\[, -Latex\\]",
            sprintf("[bend left=%s, -Latex]", bend_angle),
            edge_lines[[i]]
          )
        } else {
          edge_lines[[i]]
        }
      })
    }
  }

  # ---- Optional tier rectangles and labels ----
  tier_rect_lines <- tier_label_lines <- character(0)
  if (!is.null(tier_node_map)) {
    stopifnot(is.list(tier_node_map) && length(tier_node_map) > 0)
    tier_rect_lines <- lapply(names(tier_node_map), function(tier_name) {
      node_names <- tier_node_map[[tier_name]]
      sprintf(
        "\\node[draw, rectangle, fill=blue!20, rounded corners, inner sep=0.5cm, fit=(%s)] (%s) {};",
        paste(node_names, collapse = ")("),
        tier_name
      )
    })
    tier_label_lines <- lapply(names(tier_node_map), function(tier_name) {
      pos_info <- get_anchor_and_offset(tier_label_pos, offset = 0.2)
      sprintf(
        "\\node[anchor=%s, draw=none, fill=none] at ($(%s.%s)+(%scm,%scm)$) {%s};",
        pos_info$anchor,
        tier_name,
        pos_info$anchor_point,
        pos_info$dx,
        pos_info$dy,
        tier_name
      )
    })
  }

  # ---- TikZ global settings ----
  node_style_vec <- c()
  if (!is.null(global_node_fill)) {
    node_style_vec <- c(node_style_vec, paste0("fill=", global_node_fill))
  }
  node_style_vec <- c(
    node_style_vec,
    "circle",
    "draw",
    "minimum size=8mm",
    "align=center"
  )
  node_style_str <- paste(node_style_vec, collapse = ", ")

  generator_line <- paste0(
    "%%% Generated by causalDisco (version ",
    utils::packageVersion("causalDisco"),
    ")"
  )

  # ---- Assemble TikZ ----
  assemble_tikz <- function(doc = TRUE) {
    lines <- c(
      if (doc) {
        c(
          generator_line,
          "\\documentclass[tikz,border=2mm]{standalone}",
          "\\usetikzlibrary{arrows.meta, positioning, shapes.geometric, fit, backgrounds, calc}",
          "",
          "\\begin{document}"
        )
      } else {
        generator_line
      },

      # ---- TikZ global settings ----
      sprintf(
        "\\tikzset{arrows={[scale=%s]}, every node/.style={%s}, arrow/.style={-{Stealth}, thick}}",
        global_arrow_length,
        node_style_str
      ),

      "\\begin{tikzpicture}",

      # ---- Nodes ----
      node_lines,

      # ---- Optional tier rectangles ----
      if (length(tier_rect_lines)) {
        c(
          "\\begin{scope}[on background layer]",
          tier_rect_lines,
          "\\end{scope}"
        )
      },

      # ---- Optional tier labels ----
      tier_label_lines,

      # ---- Edges ----
      sprintf("\\path %s;", paste(edge_lines, collapse = "\n      ")),

      "\\end{tikzpicture}",
      if (doc) "\\end{document}"
    )

    paste(unlist(lines), collapse = "\n")
  }

  assemble_tikz(full_doc)
}


#' @title Generate TikZ Code from a Standard Caugi Plot
#' @inheritParams make_tikz
#' @returns A character string containing LaTeX TikZ code.
#' @keywords internal
#' @noRd
make_tikz_standard <- function(
  caugi_plot_obj,
  scale = 10,
  full_doc = TRUE,
  bend_edges = FALSE,
  bend_angle = 25
) {
  .make_tikz_core(
    caugi_plot_obj,
    scale,
    tier_node_map = NULL,
    full_doc,
    bend_edges,
    bend_angle
  )
}

#' @title Generate TikZ Code from a Tiered Caugi Plot
#' @inheritParams make_tikz
#' @returns A character string containing LaTeX TikZ code.
#' @keywords internal
#' @noRd
make_tikz_tiered <- function(
  caugi_plot_obj,
  tier_node_map,
  scale = 10,
  full_doc = TRUE,
  bend_edges = FALSE,
  bend_angle = 25,
  tier_label_pos = "above"
) {
  .make_tikz_core(
    caugi_plot_obj,
    scale,
    tier_node_map,
    full_doc,
    bend_edges,
    bend_angle,
    tier_label_pos
  )
}
