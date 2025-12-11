#' Topological ordering of a DAG given its adjacency matrix
#' @param amat An adjacency matrix.
#'
#' @return A vector of node indices in topological order.
#'
#' @keywords internal
#' @noRd
topo_order_mat <- function(amat) {
  p <- nrow(amat)

  # 'in-degree' = number of parents = number of 1s in a row
  indeg <- rowSums(amat)

  order <- integer(p)
  filled <- 0

  used <- rep(FALSE, p)

  repeat {
    candidates <- which(indeg == 0 & !used)

    if (length(candidates) == 0) {
      if (filled < p) stop("Adjacency matrix contains a cycle.")
      break
    }

    i <- candidates[1]

    filled <- filled + 1
    order[filled] <- i
    used[i] <- TRUE

    affected_children <- which(amat[, i] == 1)
    indeg[affected_children] <- indeg[affected_children] - 1

    if (filled == p) break
  }

  order
}


#' Simulate Gaussian data according to DAG
#'
#' Simulates a jointly Gaussian dataset given a DAG
#' adjacency matrix ("from-to" encoding, see \link{amat} for details).
#' The data is simulated using linear structural
#' equations and the parameters (residual standard deviations and
#' regression coefficients) are sampled from chosen intervals.
#'
#' @details A variable \eqn{X_{i}} is simulated as \cr
#' \eqn{X_{i} := \sum_{Z \in pa(X_{i})} \beta_{Z} * Z + e_{i}} \cr
#' where \eqn{pa(X_{i})} are the parents of \eqn{X_{i}} in the DAG.
#' The residual, \eqn{e_{i}}, is drawn from a normal distribution.
#'
#' @param amat An adjacency matrix.
#' @param n The number of observations that should be simulated.
#' @param regparLim The interval from which regression parameters are
#' sampled.
#' @param resSDLim The interval from which residual standard deviations
#' are sampled.
#' @param pnegRegpar The probability of sampling a negative regression
#' parameter.
#' @param standardize If \code{FALSE} (the default), the raw data are
#' returned. If \code{TRUE}, the data are first standardized, i.e.,
#' each variable will have its mean subtracted and be divided by its
#' standard deviation.
#'
#' @return A data.frame of identically distributed simulated observations from the DAG.
#'
#' @examples
#' # Simulate DAG adjacency matrix with 6 nodes
#' ex_dag <- sim_dag(6)
#'
#' # Simulate Gaussian data (100 iid observations)
#' ex_data <- sim_gaus_from_dag(ex_dag, n = 100)
#'
#' @export
sim_gaus_from_dag <- function(amat, n, regparLim = c(0.5, 2),
                              resSDLim = c(0.1, 1),
                              pnegRegpar = 0.4,
                              standardize = FALSE) {
  .check_if_pkgs_are_installed(
    pkgs = c("stats"),
    function_name = "sim_gaus_from_dag"
  )

  orig_names <- colnames(amat)
  if (is.null(orig_names)) orig_names <- paste0("X", seq_len(ncol(amat)))

  p <- nrow(amat)

  topo <- topo_order_mat(amat)
  amat_sorted <- amat[topo, topo]

  parents_list <- lapply(seq_len(p), function(i) which(amat_sorted[i, ] == 1))

  data_sorted <- matrix(0, n, p)

  residual_sd <- stats::runif(p, min = resSDLim[1], max = resSDLim[2])

  # pre-sample coefficients
  regpars <- vector("list", p)
  for (i in seq_len(p)) {
    parents <- parents_list[[i]]
    if (length(parents) > 0) {
      signs <- sample(c(-1, 1), length(parents),
        replace = TRUE,
        prob = c(pnegRegpar, 1 - pnegRegpar)
      )
      regpars[[i]] <- stats::runif(length(parents), regparLim[1], regparLim[2]) * signs
    }
  }

  # simulate exogenous variable
  data_sorted[, 1] <- stats::rnorm(n, sd = residual_sd[1])

  # simulate the rest
  for (i in 2:p) {
    parents <- parents_list[[i]]
    if (length(parents) == 0) {
      x <- stats::rnorm(n, sd = residual_sd[i])
    } else {
      x <- data_sorted[, parents, drop = FALSE] %*% regpars[[i]] +
        stats::rnorm(n, sd = residual_sd[i])
    }
    if (standardize) x <- (x - mean(x)) / stats::sd(x)
    data_sorted[, i] <- x
  }

  data_final <- data_sorted[, order(topo)]
  colnames(data_final) <- orig_names

  as.data.frame(data_final)
}
