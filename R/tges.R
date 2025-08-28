# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Public API  ──────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────


#' Estimate the restricted Markov equivalence class using Temporal Greedy Equivalence Search
#'
#' Perform causal discovery using the temporal greedy equivalence search algorithm.
#'
#' @param score tiered scoring object to be used.
#'    At the moment only scores supported are
#'    * \code{\linkS4class{TemporalBIC}} and
#'    * \code{\linkS4class{TemporalBDeu}}.
#' @param verbose	indicates whether debug output should be printed.
#'
#' @author Tobias Ellegaard Larsen
#'
#' @return A `discography` object (the canonical representation for
#'   `EssGraph`/`TEssGraph` results in this package).
#'
#'
#' @export
tges <- function(score, verbose = FALSE) {
  if (!inherits(score, c("TemporalBIC", "TemporalBDeu"))) {
    stop("Score must be of type TemporalBIC or TemporalBDeu, ",
      "the only score criteria supported by tges at the moment.",
      call. = FALSE
    )
  }
  if (inherits(score, "TemporalBDeu") &&
    !all(vapply(score$pp.dat$data, is.factor, logical(1)))) {
    stop("When using TemporalBDeu the data must be factors.", call. = FALSE)
  }
  if (anyNA(score$pp.dat$data)) {
    stop("Data must not contain missing values.", call. = FALSE)
  }
  node.numbers <- 1:score$pp.dat$vertex.count
  essgraph <- new("TEssGraph",
    nodes = as.character(node.numbers),
    score = score
  )
  Forbidden.edges <- essgraph$.in.edges # all with integer(0) entry (list)
  node.names <- score$.nodes
  num.bidir <- 0
  num.directed <- 0

  ord <- score$.order
  for (n in node.numbers) {
    Forbidden.edges[[n]] <- node.numbers[ord[n] < ord]
  }

  cont <- TRUE
  while (cont) {
    cont <- FALSE

    # Forward phase
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("forward", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) {
        cont <- TRUE
      } else {
        break
      }

      for (i in names(tempstep[-1])) { # Run through the nodes that have been changed
        in.node.edges <- tempstep[-1][[i]] # save the in.node edges of node i
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges] # List of edges to be removed
        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(gInput = create_adj_matrix_from_list(essgraph$.in.edges), x = bgx, y = bgy, verbose = verbose)
          no.forbidden.edges <- create_list_from_adj_matrix(amatbg)
          essgraph$.in.edges <- no.forbidden.edges
        }
      }
    }

    # Backward phase
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("backward", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) {
        cont <- TRUE
      } else {
        break
      }

      for (i in names(tempstep[-1])) { # Run through the nodes that have been changed
        in.node.edges <- tempstep[-1][[i]] # save the in.node edges of node i
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges] # List of edges to be removed
        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(gInput = create_adj_matrix_from_list(essgraph$.in.edges), x = bgx, y = bgy, verbose = verbose)
          no.forbidden.edges <- create_list_from_adj_matrix(amatbg)
          essgraph$.in.edges <- no.forbidden.edges
        }
      }
    }

    # Turning phase
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("turning", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) {
        cont <- TRUE
      } else {
        break
      }

      for (i in names(tempstep[-1])) { # Run through the nodes that have been changed
        in.node.edges <- tempstep[-1][[i]] # save the in.node edges of node i
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges] # List of edges to be removed
        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(gInput = create_adj_matrix_from_list(essgraph$.in.edges), x = bgx, y = bgy, verbose = verbose)
          no.forbidden.edges <- create_list_from_adj_matrix(amatbg)
          essgraph$.in.edges <- no.forbidden.edges
        }
      }
    }
  }

  return(essgraph |> discography())
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Helpers  ────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────


#' Create adjacency matrix from an in-edge list
#'
#' Converts a list of in-edges (parents) per node into a 0/1 adjacency matrix
#' with rows and columns ordered by the list names.
#'
#' @param inputList A named list of integer vectors; each element `inputList[[i]]`
#'   contains the indices of the parents of node `i`. Names must be the node
#'   labels in order.
#'
#' @return A numeric matrix with row and column names equal to `names(inputList)`,
#'   where entry `(i, j) = 1` iff `j` is a parent of `i`.
#'
#' @keywords internal
create_adj_matrix_from_list <- function(inputList) {
  n <- length(inputList)
  resultMatrix <- matrix(0, nrow = n, ncol = n)
  for (i in seq_along(inputList)) {
    indices <- inputList[[i]]
    validIndices <- indices[indices <= n]
    if (length(validIndices) > 0) {
      resultMatrix[i, validIndices] <- 1
    }
  }
  rownames(resultMatrix) <- names(inputList)
  colnames(resultMatrix) <- names(inputList)
  resultMatrix
}

#' Create in-edge list from an adjacency matrix
#'
#' Converts a 0/1 adjacency matrix into a list where each element contains the
#' integer indices of the parents of the corresponding row/node.
#'
#' @param adjMatrix A square numeric matrix with row and column names giving
#'   node labels. Entry `(i, j) = 1` indicates an edge `j -> i`.
#'
#' @return A named list of integer vectors with one element per node; each
#'   element lists the parent indices (columns with value 1 in that row).
#'
#' @keywords internal
create_list_from_adj_matrix <- function(adjMatrix) {
  n <- nrow(adjMatrix)
  resultList <- vector("list", n)
  names(resultList) <- rownames(adjMatrix)
  for (i in seq_len(n)) {
    connectedIndices <- as.integer(which(adjMatrix[i, ] == 1))
    resultList[[i]] <- if (length(connectedIndices) > 0) connectedIndices else integer(0)
  }
  resultList
}

#' Normalize outputs to an adjacency matrix
#'
#' Accepts several return types from `pcalg::addBgKnowledge()` and converts them
#' to an adjacency matrix if possible.
#'
#' @param obj One of:
#'   * `NULL` (returns `NULL`),
#'   * a numeric matrix (returned unchanged),
#'   * a `graphNEL` object,
#'   * an S4 object with a `graph` slot that contains a `graphNEL`.
#'
#' @return A numeric adjacency matrix, or `NULL` if `obj` cannot be normalized.
#'
#' @keywords internal
to_adj_mat <- function(obj) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods", "pcalg"
    ),
    function_name = "to_adj_mat"
  )

  if (is.matrix(obj)) {
    return(obj)
  }
  if (inherits(obj, "graphNEL")) {
    return(graph2amat(obj, toFrom = FALSE))
  }
  # pcAlgo-like: unwrap @graph if present
  if (isS4(obj)) {
    if ("graph" %in% methods::slotNames(obj)) {
      g <- methods::slot(obj, "graph")
      if (inherits(g, "graphNEL")) {
        return(graph2amat(g, toFrom = FALSE))
      }
    }
  }
  NULL
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────── Graph / Score  ─────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Temporal EssGraph class with greedy steps
#'
#' A `RefClass` extending `EssGraph` that exposes a single-step greedy move
#' (`forward`, `backward`, or `turning`) through `greedy.step()`. Used by
#' [tges()] to iterate GIES one step at a time and interleave background
#' knowledge enforcement.
#'
#' @section Methods:
#' \describe{
#'   \item{greedy.step(direction = c("forward","backward","turning"), verbose = FALSE, ...)}{
#'     Performs a single improving step in the requested phase. Returns a named
#'     vector/list where the first element indicates whether a step happened,
#'     followed by any nodes whose in-edges changed.
#'   }
#' }
#'
#' @seealso [tges()], [GaussL0penIntScoreORDER]
#' @importClassesFrom pcalg EssGraph
#' @importFrom methods new
#' @export
TEssGraph <- setRefClass("TEssGraph",
  contains = "EssGraph",
  methods = list(
    # Performs one greedy step
    greedy.step = function(direction = c("forward", "backward", "turning"), verbose = FALSE, ...) {
      stopifnot(!is.null(score <- getScore()))

      # Cast direction
      direction <- match.arg(direction)
      alg.name <- switch(direction,
        forward = "GIES-F",
        backward = "GIES-B",
        turning = "GIES-T"
      )

      new_graph <- .Call(
        causalInference,
        .in.edges,
        score$pp.dat,
        alg.name,
        score$c.fcn,
        causal.inf.options(
          caching = FALSE,
          maxSteps = 1,
          verbose = verbose,
          ...
        )
      )
      if (identical(new_graph, "interrupt")) {
        return(FALSE)
      }

      if (new_graph$steps > 0) {
        last.edges <- .in.edges
        .in.edges <<- new_graph$in.edges
        names(.in.edges) <<- .nodes

        new_in.edges <- .in.edges[sapply(names(.in.edges), function(x) !identical(.in.edges[[x]], last.edges[[x]]))]
      } else {
        new_in.edges <- list()
      }


      return(c((new_graph$steps == 1), new_in.edges))
    }
  ), inheritPackage = TRUE
)

#' Temporal Bayesian Information Criterion (Score criterion)
#'
#' A Reference Class for Gaussian Observational Data Scoring with Tiered
#' Background Knowledge. This class represents a score for causal discovery
#' using tiered background knowledge from observational Gaussian
#' data; it is used in the causal discovery function \code{\link{tges}}.
#'
#' The class implements a score which scores all edges contradicting the ordering
#' (edge going from a later tier to an earlier) to minus \eqn{\infty}{∞}. If the
#' the edges does not contradict, the score is equal to that of \code{\linkS4class{GaussL0penObsScore}}:
#' The class implements an \eqn{\ell_0}{ℓ0}-penalized Gaussian maximum
#' likelihood estimator. The penalization is a constant (specified by
#' the argument \code{lambda} in the constructor) times the number of
#' parameters of the DAG model. By default, the constant \eqn{\lambda}{λ} is
#' chosen as \eqn{\log(n)/2}{log(n)/2}, which corresponds to the BIC score.
#'
#' @section Extends:
#' Class \code{\linkS4class{GaussL0penObsScore}} from \pkg{pcalg}, directly.
#'
#' All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
#'
#' @section Constructor:
#' \preformatted{
#' new("TemporalBIC",
#'   data = matrix(1, 1, 1),
#'   order =  rep(1,ncol(data)),
#'   lambda = 0.5 * log(nrow(data)),
#'   intercept = TRUE,
#'   ...)
#' }
#'
#' @param data A numeric matrix with \eqn{n} rows and \eqn{p} columns. Each row
#' corresponds to one observational realization.
#' @param order A vector specifying the order each variable. Can be either a vector of integers
#' or an vector of prefixes. If integers, such that the ith entry
#' will detail the order of the ith variable in the dataset. Must start at 1 an increase
#' with increments of 1. If prefixes, must be in order.
#' @param lambda Penalization constant (see details).
#' @param intercept Logical; indicates whether an intercept is allowed in the
#' linear structural equations (i.e., whether a nonzero mean is allowed).
#'
#' @author Tobias Ellegaard Larsen
#'
#' @examples
#' # Simulate Gaussian data
#' set.seed(123)
#' n <- 500
#' child_x <- rnorm(n)
#' child_y <- 0.5 * child_x + rnorm(n)
#' child_z <- 2 * child_x + child_y + rnorm(n)
#'
#' adult_x <- child_x + rnorm(n)
#' adult_z <- child_z + rnorm(n)
#' adult_w <- 2 * adult_z + rnorm(n)
#' adult_y <- 2 * child_x + adult_w + rnorm(n)
#'
#' simdata <- data.frame(
#'   child_x, child_y, child_z,
#'   adult_x, adult_z, adult_w,
#'   adult_y
#' )
#'
#' # Define order in prefix way
#' prefix_order <- c("child", "adult")
#'
#' # Define TBIC score
#' t_score <- new("TemporalBIC",
#'   order = prefix_order,
#'   data = simdata
#' )
#' # Run tges
#' tges_pre <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_pre)
#'
#' # Define order in integer way
#' integer_order <- c(1, 1, 1, 2, 2, 2, 2)
#'
#' # Define TBIC score
#' t_score <- new("TemporalBIC",
#'   order = integer_order,
#'   data = simdata
#' )
#' # Run tges
#' tges_int <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_int)
#'
#' @seealso
#' \code{\link{tges}}
#'
#' @importClassesFrom pcalg GaussL0penIntScore
#'
#' @export
TemporalBIC <- setRefClass("TemporalBIC",
  contains = "GaussL0penIntScore",
  fields = list(
    .order = "vector"
  ),
  methods = list(
    initialize = function(data = NULL,
                          nodes = colnames(data),
                          lambda = 0.5 * log(nrow(data)),
                          intercept = TRUE,
                          format = c("raw", "scatter"),
                          knowledge = NULL,
                          order = NULL, # deprecated
                          ...) {
      if (!is.null(knowledge) && !is.null(order)) {
        stop("Both `knowledge` and `order` supplied. ",
          "Please supply a knowledge object only.",
          call. = FALSE
        )
      }
      if (is.null(knowledge) && !is.null(order)) {
        warning(
          "`order` is deprecated in version 1.0.0 and will be removed in",
          " a future version. Please supply a `knowledge` object instead."
        )
        if (is.numeric(order)) {
          knowledge <- rlang::inject(knowledge(data, tier(!!order)))
        } else if (is.character(order)) {
          knowledge <- causalDisco:::.build_knowledge_from_order(
            order,
            vnames = nodes
          )
        } else {
          stop("`order` must be either a vector of integers or a vector of ",
            "prefixes. Provide a knowledge object instead.",
            call. = FALSE
          )
        }
      }
      if (is.null(knowledge) && is.null(order)) {
        knowledge <- knowledge() |> add_vars(nodes)
      }

      causalDisco:::check_knowledge_obj(knowledge)

      .order <<- causalDisco:::.tier_index(knowledge, nodes)

      callSuper(
        data = data,
        targets = list(integer(0)),
        target.index = rep(as.integer(1), nrow(data)),
        nodes = nodes,
        lambda = lambda,
        intercept = intercept,
        format = format,
        ...
      )
    },
    local.score = function(vertex, parents, ...) {
      validate.vertex(vertex)
      validate.parents(parents)

      ord <- .order
      child_t <- ord[vertex]
      parent_ts <- ord[parents]

      # do not enforce if child or any parent lacks a tier (NA)
      if (is.na(child_t) || any(is.na(parent_ts)) ||
        child_t >= max(c(parent_ts, -Inf))) {
        if (c.fcn == "none") {
          # calculate score in R
          if (.format == "raw") {
            # calculate score from raw data matrix
            # response vector for linear regression
            Y <- pp.dat$data[pp.dat$non.int[[vertex]], vertex]
            sigma2 <- sum(Y^2)

            if (length(parents) + pp.dat$intercept != 0) {
              # get data matrix on which linear regression is based
              Z <- pp.dat$data[pp.dat$non.int[[vertex]], parents, drop = FALSE]
              if (pp.dat$intercept) {
                Z <- cbind(1, Z)
              }

              # calculate the scaled error covariance using QR decomposition
              Q <- qr.Q(qr(Z))
              sigma2 <- sigma2 - sum((Y %*% Q)^2)
            }
          } else if (.format == "scatter") {
            # calculate the score based on pre-calculated scatter matrices
            # if an intercept is allowed, add a fake parent node
            parents <- sort(parents)
            if (pp.dat$intercept) {
              parents <- c(pp.dat$vertex.count + 1, parents)
            }

            pd.scMat <- pp.dat$scatter[[pp.dat$scatter.index[vertex]]]
            sigma2 <- pd.scMat[vertex, vertex]
            if (length(parents) != 0) {
              b <- pd.scMat[vertex, parents]
              sigma2 <- sigma2 - as.numeric(b %*% solve(pd.scMat[parents, parents], b))
            }
          }

          # return local score
          lscore <- -0.5 * pp.dat$data.count[vertex] *
            (1 + log(sigma2 / pp.dat$data.count[vertex])) -
            pp.dat$lambda * (1 + length(parents))
          return(lscore)
        }
      } else {
        skip <- -Inf
        return(skip)
      } # set score to minus infinity if vertex earlier than parents
    }
  ),
  inheritPackage = TRUE
)

#' Temporal Bayesian Dirichlet equivalent uniform (Score criterion)
#'
#' A reference class for categorical observational data Scoring with Tiered
#' Background Knowledge. This class represents a score for causal discovery
#' using tiered background knowledge from observational categorical
#' data; it is used in the causal discovery function \code{\link{tges}}.
#'
#' The class implements a score which scores all edges contradicting the ordering
#' (edge going from a later tier to an earlier) to minus \eqn{\infty}{∞}. If the
#' the edges does not contradict, the score is equal to that of the standard BDeu.
#'
#' @section Extends:
#' Class \code{\linkS4class{Score}} from \pkg{pcalg}, directly.
#'
#' All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
#'
#' @section Constructor:
#' \preformatted{
#' new("TemporalBdeu",
#'   data = matrix(1, 1, 1),
#'   order =  rep(1,ncol(data)),
#'   iss = 1
#'   ...)
#' }
#'
#'
#'
#' @param data A numeric matrix with \eqn{n} rows and \eqn{p} columns. Each row
#' corresponds to one observational realization.
#' @param order A vector specifying the order each variable. Can be either a vector of integers
#' or an vector of prefixes. If integers, such that the ith entry
#' will detail the order of the ith variable in the dataset. Must start at 1 an increase
#' with increments of 1. If prefixes, must be in order.
#' @param iss Imaginary Sample Size (ISS), also referred to as
#' Equivalent Sample Size (ESS), determines how much weight is assigned to the prior
#' in terms of the size of animaginary sample supporting it. Increasing the ISS will
#' increase the density of the estimated graph.
#'
#' @author Tobias Ellegaard Larsen
#'
#' @examples
#' # For reproducibility
#' set.seed(123)
#'
#' # Number of samples
#' n <- 1000
#'
#' # Define probabilities for A
#' p_A <- c(0.4, 0.35, 0.25) # Probabilities for A = {1, 2, 3}
#'
#' # Simulate A from a categorical distribution
#' A <- sample(1:3, n, replace = TRUE, prob = p_A)
#'
#' # Define conditional probabilities for B given A
#' p_B_given_A <- list(
#'   c(0.7, 0.3), # P(B | A=1)
#'   c(0.4, 0.6), # P(B | A=2)
#'   c(0.2, 0.8) # P(B | A=3)
#' )
#'
#' # Sample B based on A
#' B <- sapply(A, function(a) sample(1:2, 1, prob = p_B_given_A[[a]]))
#'
#' # Define conditional probabilities for C given A and B
#' p_C_given_A_B <- list(
#'   "1_1" = c(0.6, 0.4), # P(C | A=1, B=1)
#'   "1_2" = c(0.3, 0.7), # P(C | A=1, B=2)
#'   "2_1" = c(0.5, 0.5), # P(C | A=2, B=1)
#'   "2_2" = c(0.2, 0.8), # P(C | A=2, B=2)
#'   "3_1" = c(0.7, 0.3), # P(C | A=3, B=1)
#'   "3_2" = c(0.4, 0.6) # P(C | A=3, B=2)
#' )
#'
#' # Sample C based on A and B
#' C <- mapply(function(a, b) sample(1:2, 1, prob = p_C_given_A_B[[paste(a, b, sep = "_")]]), A, B)
#'
#' # Create dataset
#' simdata <- data.frame(as.factor(A), as.factor(B), as.factor(C))
#'
#' # Define order in prefix way
#' colnames(simdata) <- c("child_A", "child_B", "adult_C")
#' prefix_order <- c("child", "adult")
#'
#' # Define TemporalBDeu score
#' t_score <- new("TemporalBDeu",
#'   order = prefix_order,
#'   data = simdata
#' )
#' # Run tges
#' tges_pre <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_pre)
#'
#'
#' # Define order in integer way
#' colnames(simdata) <- c("A", "B", "C")
#' integer_order <- c(1, 1, 2)
#'
#' # Define TemporalBDeu score
#' t_score <- new("TemporalBDeu",
#'   order = integer_order,
#'   data = simdata
#' )
#' # Run tges
#' tges_integer <- tges(t_score)
#'
#' # Plot MPDAG
#' plot(tges_integer)
#'
#' @seealso
#' \code{\link{tges}}
#'
#' @importClassesFrom pcalg Score
#'
#' @export
TemporalBDeu <- setRefClass("TemporalBDeu",
  contains = "DataScore",
  fields = list(
    .order = "vector",
    .iss = "numeric"
  ),
  methods = list(
    initialize = function(data = matrix(1, 1, 1),
                          nodes = colnames(data),
                          iss = 1,
                          knowledge = NULL,
                          order = NULL, # deprecated
                          ...) {
      if (!is.null(knowledge) && !is.null(order)) {
        stop("Both `knowledge` and `order` supplied. ",
          "Please supply a knowledge object only.",
          call. = FALSE
        )
      }
      if (is.null(knowledge) && !is.null(order)) {
        warning(
          "`order` is deprecated in version 1.0.0 and will be removed in",
          " a future version. Please supply a `knowledge` object instead."
        )
        if (is.numeric(order)) {
          knowledge <- rlang::inject(knowledge(data, tier(!!order)))
        } else if (is.character(order)) {
          knowledge <- causalDisco:::.build_knowledge_from_order(
            order,
            vnames = nodes
          )
        } else {
          stop("`order` must be either a vector of integers or a vector of ",
            "prefixes. Provide a knowledge object instead.",
            call. = FALSE
          )
        }
      }
      if (is.null(knowledge) && is.null(order)) {
        knowledge <- knowledge() |> add_vars(nodes)
      }

      causalDisco:::check_knowledge_obj(knowledge)

      .order <<- causalDisco:::.tier_index(knowledge, nodes)
      .iss <<- iss

      callSuper(
        data = data,
        nodes = nodes,
        iss = iss,
        ...
      )
    },
    local.score = function(vertex, parents, ...) {
      # check validity of arguments
      validate.vertex(vertex)
      validate.parents(parents)
      ord <- .order
      iss <- .iss
      child_t <- ord[vertex]
      parent_ts <- ord[parents]

      # do not enforce if child or any parent lacks a tier (NA)
      if (is.na(child_t) || any(is.na(parent_ts)) ||
        child_t >= max(c(parent_ts, -Inf))) {
        D <- pp.dat$data[, c(vertex, parents), drop = FALSE]
        pa_nam <- colnames(pp.dat$data)[parents]
        ve_nam <- colnames(pp.dat$data)[vertex]

        if (length(parents) == 0) {
          # number of possible configurations of states of parents
          q <- 1

          # number of states for vertex
          r <- nlevels(D[[ve_nam]])

          alpha_j <- iss

          # set pa_score to 0
          pa_score <- lgamma(alpha_j) - lgamma(alpha_j + nrow(D))
        } else {
          # number of in variables
          nlev_D <- sapply(D[, c(ve_nam, pa_nam)], nlevels)

          # number of possible configurations of states of parents
          q <- prod(nlev_D[pa_nam])

          # number of states for vertex
          r <- nlev_D[ve_nam]

          # table with number of occurences for parents and parents combinations
          tab_pa <- as.data.frame(table(D[, c(pa_nam)]))

          alpha_j <- iss / q

          pa_score <- nrow(tab_pa) * lgamma(alpha_j) -
            sum(sapply(tab_pa$Freq, function(x) lgamma(alpha_j + x)))
        }


        # table with number of occurences and state combinations
        tab_D <- as.data.frame(table(D))


        # uniform prior with alpha according to imaginary sample size (iss)
        alpha_jk <- alpha_j / r

        constant <- nrow(tab_D) * lgamma(alpha_jk) + pa_score
        BdeuScore <- sum(
          sapply(
            tab_D$Freq,
            function(x) lgamma(alpha_jk + x)
          )
        ) - constant



        return(BdeuScore)
      } else {
        skip <- -Inf
        return(skip)
      }
    }
  ),
  inheritPackage = TRUE
)
