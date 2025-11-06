#' Build a lightweight S3 wrapper around a Tetrad PAG string
#'
#' @param x Character scalar in the form
#'   \verb{"V1,V2,V3\\n0,2,0\\n3,0,1\\n0,3,0\\n"}
#'
#' @example inst/roxygen-examples/tetrad_graph_example.R
#'
#' @return An object of class <tetrad_graph>
#'
#' @export
tetrad_graph <- function(x) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "readr", "stringr"
    ),
    function_name = "tetrad_graph"
  )

  if (!is.character(x) || length(x) != 1) {
    stop("`x` must be a single character string")
  }

  parts <- stringr::str_split(x, "\n")[[1]] |>
    (\(v) v[v != ""])()

  nodes <- stringr::str_split(parts[1], ",", simplify = TRUE) |>
    as.character()

  mat_text <- paste(parts[-1], collapse = "\n")

  amat <- readr::read_csv(mat_text,
    col_names = FALSE,
    show_col_types = FALSE
  ) |>
    as.matrix()

  storage.mode(amat) <- "integer"
  class(amat) <- c("matrix")
  if (nrow(amat) != ncol(amat)) {
    stop("Adjacency matrix must be square")
  }
  dimnames(amat) <- list(nodes, nodes)

  structure(list(nodes = nodes, amat = amat),
    class = "tetrad_graph"
  )
}
