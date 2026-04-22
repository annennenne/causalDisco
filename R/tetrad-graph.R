#' Build a lightweight S3 wrapper around a Tetrad PAG string
#'
#' @param x Character scalar in the form
#'   \verb{"V1,V2,V3\\n0,2,0\\n3,0,1\\n0,3,0\\n"}
#'
#' @example inst/roxygen-examples/tetrad_graph-example.R
#'
#' @return An object of class <tetrad_graph>
#'
#' @keywords internal
#' @noRd
tetrad_graph <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("`x` must be a single character string")
  }

  parts <- strsplit(x, "\n")[[1]]
  parts <- parts[parts != ""]

  nodes <- strsplit(parts[1], ",")[[1]]
  nodes <- as.character(nodes)

  mat_text <- paste(parts[-1], collapse = "\n")
  amat <- utils::read.csv(
    text = mat_text,
    header = FALSE
  ) |>
    as.matrix()

  storage.mode(amat) <- "integer"
  class(amat) <- c("matrix")
  if (nrow(amat) != ncol(amat)) {
    stop("Adjacency matrix must be square")
  }
  dimnames(amat) <- list(nodes, nodes)

  structure(list(nodes = nodes, amat = amat), class = "tetrad_graph")
}
