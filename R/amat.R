#' Extract adjacency matrix from tpdag, cpdag, tpag or pag object
#'
#' If the input is a tpdag or cpdag, the resulting adjacency matrix A is
#' "to-from" matrix encoded as follows:
#'  - A(i,j) = 1 and A(j,i) = 0 means there is an edge j -> i.
#'  - A(j,i) = 1 and A(i,j) = 0 means there is an edge i -> j.
#'  - A(i,j) = 1 and A(j,i) = 1 means there is an undirected edge between i
#'    and j, i - j.
#'  - A(i,j) = 0 and A(j,i) = 0 means there is no edge between i and j.
#'
#' If the inout is a tpag or pag, there are four possible entry values: 0 (no edge), 1 (circle),
#' 2 (arrowhead), 3 (tail). It is still encoded as a "to-from" adjacency matrix, which means
#' that e.g. A(i,j) = 1 places a circle in the directed from j to i. For example, if
#' A(i,j) = 1 and A(j,i) = 2, we have that i o-> j. Similarly, A(i,j) = 2 and A(j,i) = 3
#' mean that i <- j.
#'
#' @param x \code{tpdag}, \code{cpdag}, \code{tpag}, \code{pag}, or
#' \code{caugi} object as obtained from \link{tpc}, \link{pc},
#' \link{tfci}, or \link{fci}, respectively.
#'
#' @keywords internal
amat <- function(x) {
  x_class <- class(x)

  if (any(c("tpdag", "tpag") %in% x_class)) {
    out <- x$tamat
  } else if (any(c("cpdag", "pag") %in% x_class)) {
    out <- x$amat
  } else if ("caugi" %in% x_class) {
    # infer node order
    nodes <- x@nodes$name
    n <- length(nodes)
    idx <- stats::setNames(seq_along(nodes), nodes)

    # detect PAG-style marks
    pag_marks <- c("<->", "o-o", "--o", "o->")
    is_pag <- any(x@edges$edge %in% pag_marks)

    if (!is_pag) {
      # cpdag encoding: 0/1 "from-to"
      adj_mat <- matrix(0L, n, n, dimnames = list(nodes, nodes))
      # directed -->
      dir_rows <- x@edges$edge == "-->"
      if (any(dir_rows)) {
        fr <- idx[x@edges$from[dir_rows]]
        to <- idx[x@edges$to[dir_rows]]
        adj_mat[cbind(to, fr)] <- 1L
      }
      # undirected ---
      und_rows <- x@edges$edge == "---"
      if (any(und_rows)) {
        fr <- idx[x@edges$from[und_rows]]
        to <- idx[x@edges$to[und_rows]]
        adj_mat[cbind(fr, to)] <- 1L
        adj_mat[cbind(to, fr)] <- 1L
      }
      class(adj_mat) <- c("amat.cpdag", "matrix")
      out <- adj_mat
    } else {
      # pag encoding: 0 none, 1 circle, 2 arrow, 3 tail
      code_pair <- function(type) {
        switch(
          type,
          "-->" = c(3L, 2L), # from tail, to arrow
          "---" = c(3L, 3L), # tail, tail
          "<->" = c(2L, 2L), # arrow, arrow
          "o-o" = c(1L, 1L), # circle, circle
          "--o" = c(3L, 1L), # tail, circle
          "o->" = c(1L, 2L), # circle, arrow
          c(0L, 0L)
        )
      }
      adj_mat <- matrix(0L, n, n, dimnames = list(nodes, nodes))
      for (k in seq_len(nrow(x))) {
        f <- x@edges$from[[k]]
        t <- x@edges$to[[k]]
        if (!nzchar(f) || !nzchar(t) || is.na(f) || is.na(t) || f == t) {
          next
        }
        codes <- code_pair(x@edges$edge[[k]])
        i <- idx[[t]]
        j <- idx[[f]]
        # entry [i, j] is the mark at i on the edge j -> i
        adj_mat[i, j] <- max(adj_mat[i, j], codes[1])
        adj_mat[j, i] <- max(adj_mat[j, i], codes[2])
      }
      class(adj_mat) <- c("amat.pag", "matrix")
      out <- adj_mat
    }
  }

  out
}
