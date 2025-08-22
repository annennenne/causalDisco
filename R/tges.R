# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Public API  ──────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────


#' Temporal GES with background knowledge
#'
#' Runs a greedy equivalence search (GES) over essential graphs while enforcing
#' temporal background knowledge. The search proceeds in three phases
#' (forward, backward, turning), cleaning up any edges that violate the supplied
#' knowledge after each improving step.
#'
#' @param score A score object compatible with \pkg{pcalg}/\pkg{gies}, typically
#'   an instance of `GaussL0penIntScore` or `GaussL0penIntScoreORDER`.
#'   Must provide `score$pp.dat$vertex.count` (scalar integer) and `score$.nodes`.
#' @param knowledge A knowledge object describing tiers/temporal ordering of
#'   variables. Parents from later tiers are forbidden. See \link{knowledge}.
#' @param order Deprecated. A character vector of tier labels used to build a
#'   `knowledge` object via prefix matching of node names. Use `knowledge=`
#'   instead.
#' @param verbose Logical; if `TRUE`, passes verbosity to the C++ search and
#'   background-knowledge application.
#'
#' @details
#' The function constructs forbidden parent sets per node from `knowledge` and
#' runs GIES one step at a time in each phase. After any improving step, it
#' applies `pcalg::addBgKnowledge()` to re-impose the tier constraints and
#' updates the in-edge representation accordingly.
#'
#' @return A `discography` object (the canonical representation for
#'   `EssGraph`/`TEssGraph` results in this package).
#'
#' @seealso [GaussL0penIntScoreORDER], [knowledge()], [pcalg::addBgKnowledge()]
#'
#' @export
tges <- function(score,
                 knowledge = NULL,
                 order = NULL, # deprecated
                 verbose = FALSE) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "tges"
  )

  # ---------------------------------------------------------------------------
  # Arg handling: prefer `knowledge`, allow legacy `order` with a warning
  # ---------------------------------------------------------------------------
  if (!is.null(knowledge) && !is.null(order)) {
    stop(
      "Both `knowledge` and `order` supplied. ",
      "Please supply a knowledge object."
    )
  }
  if (is.null(knowledge) && !is.null(order)) {
    warning(
      "`order` is deprecated in version 1.0.0 and will be removed in a ",
      "future version. Please supply a `knowledge` object instead."
    )
    knowledge <- .build_knowledge_from_order(
      order,
      vnames = score$.nodes
    )
  }

  check_knowledge_obj(knowledge)


  # ---------------------------------------------------------------------------
  # Validate score object before using it
  # ---------------------------------------------------------------------------
  if (is.null(score$pp.dat) ||
    is.null(score$pp.dat$vertex.count) ||
    !is.numeric(score$pp.dat$vertex.count) ||
    length(score$pp.dat$vertex.count) != 1L ||
    is.null(score$.nodes)) {
    stop(
      "Invalid `score` object supplied: must have ",
      "`score$pp.dat$vertex.count` (scalar integer) and `.nodes`."
    )
  }

  # ---------------------------------------------------------------------------
  # Basic score-derived quantities
  # ---------------------------------------------------------------------------
  node.names <- score$.nodes
  node.numbers <- seq_len(score$pp.dat$vertex.count)

  # ---------------------------------------------------------------------------
  # Initialize graph / forbidden parent sets from temporal knowledge
  # ---------------------------------------------------------------------------
  essgraph <- new("TEssGraph", nodes = as.character(node.numbers), score = score)

  # Forbidden incoming edges are from *later* temporal tiers
  Forbidden.edges <- essgraph$.in.edges
  tier_idx <- .tier_index(knowledge, node.names)

  for (i in node.numbers) {
    # all nodes with strictly higher tier rank than node i are forbidden as parents of i
    later_nodes <- node.numbers[which(tier_idx > tier_idx[i])]
    Forbidden.edges[[i]] <- later_nodes
  }

  # ---------------------------------------------------------------------------
  # Greedy search (forward, backward, turning) with clean-up of forbidden edges
  # ---------------------------------------------------------------------------
  cont <- TRUE
  while (cont) {
    cont <- FALSE

    # ----- forward phase ------------------------------------------------------
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("forward", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) cont <- TRUE

      for (i in names(tempstep[-1])) {
        in.node.edges <- tempstep[-1][[i]]
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges]

        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(
            gInput  = create_adj_matrix_from_list(essgraph$.in.edges),
            x       = bgx,
            y       = bgy,
            verbose = verbose
          )
          amatbg <- to_adj_mat(amatbg)
          if (!is.null(amatbg)) {
            no.forbidden.edges <- create_list_from_adj_matrix(amatbg)
            essgraph$.in.edges <- no.forbidden.edges
          }
        }
      }
    }

    # ----- backward phase -----------------------------------------------------
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("backward", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) cont <- TRUE

      for (i in names(tempstep[-1])) {
        in.node.edges <- tempstep[-1][[i]]
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges]

        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(
            gInput  = create_adj_matrix_from_list(essgraph$.in.edges),
            x       = bgx,
            y       = bgy,
            verbose = verbose
          )
          amatbg <- to_adj_mat(amatbg)
          if (!is.null(amatbg)) {
            no.forbidden.edges <- create_list_from_adj_matrix(amatbg)
            essgraph$.in.edges <- no.forbidden.edges
          }
        }
      }
    }

    # ----- turning phase ------------------------------------------------------
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step("turning", verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (runwhile) cont <- TRUE

      for (i in names(tempstep[-1])) {
        in.node.edges <- tempstep[-1][[i]]
        forbidden.node.edges <- Forbidden.edges[[as.numeric(i)]]
        removed.edges <- in.node.edges[in.node.edges %in% forbidden.node.edges]

        if (length(removed.edges) > 0) {
          bgx <- rep(as.numeric(i), length(removed.edges))
          bgy <- removed.edges
          amatbg <- pcalg::addBgKnowledge(
            gInput  = create_adj_matrix_from_list(essgraph$.in.edges),
            x       = bgx,
            y       = bgy,
            verbose = verbose
          )
          amatbg <- to_adj_mat(amatbg)
          if (!is.null(amatbg)) {
            no.forbidden.edges <- create_list_from_adj_matrix(amatbg)
            essgraph$.in.edges <- no.forbidden.edges
          }
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Finalize names and return as discography (method for EssGraph/TEssGraph)
  # ---------------------------------------------------------------------------
  essgraph$.nodes <- node.names
  names(essgraph$.in.edges) <- node.names

  essgraph |> discography()
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
#'
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

      new.graph <- .Call(
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
      if (identical(new.graph, "interrupt")) {
        return(FALSE)
      }

      if (new.graph$steps > 0) {
        last.edges <- .in.edges
        .in.edges <<- new.graph$in.edges
        names(.in.edges) <<- .nodes

        new.in.edges <- .in.edges[sapply(names(.in.edges), function(x) !identical(.in.edges[[x]], last.edges[[x]]))]
      } else {
        new.in.edges <- list()
      }


      return(c((new.graph$steps == 1), new.in.edges))
    }
  ), inheritPackage = TRUE
)


#' Order-aware Gauss L0 penalized score
#'
#' Extends `GaussL0penIntScore` with an `.order` field and enforces that a
#' vertex may only have parents from the same or earlier order/tier. When
#' `.format == "raw"` or `"scatter"` and `c.fcn == "none"`, the local score is
#' computed in R; otherwise it delegates to the C++ scoring function.
#'
#' @section Fields:
#' \describe{
#'   \item{.order}{Numeric or integer vector, one per node, giving the allowed
#'   temporal/tier order. Parents must not have a strictly larger value than the
#'   child.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(..., order = rep(1, p))}{Constructor that stores the order
#'   and forwards remaining arguments to `GaussL0penIntScore`.}
#'   \item{local.score(vertex, parents, ...)}{Returns the local contribution of
#'   `vertex` given `parents`. Returns `-Inf` if any parent violates the order.}
#' }
#'
#' @seealso [tges()]
#'
#' @export
GaussL0penIntScoreORDER <- setRefClass("GaussL0penIntScoreORDER",
  contains = "GaussL0penIntScore",
  fields = list(
    .order = "vector"
  ),
  methods = list(
    # Constructor
    initialize = function(data = matrix(1, 1, 1),
                          nodes = colnames(data),
                          lambda = 0.5 * log(nrow(data)),
                          intercept = TRUE,
                          format = c("raw", "scatter"),
                          use.cpp = TRUE,
                          order = rep(1, ncol(data)),
                          ...) {
      .order <<- order
      callSuper(
        data = data,
        targets = list(integer(0)),
        target.index = rep(as.integer(1), nrow(data)),
        nodes = nodes,
        lambda = lambda,
        intercept = intercept,
        format = format,
        use.cpp = use.cpp,
        ...
      )
    }, # Same as GaussL0penObsScore

    # Calculates the local score of a vertex and its parents
    local.score = function(vertex, parents, ...) {
      ## Check validity of arguments
      validate.vertex(vertex)
      validate.parents(parents)
      order <- .order
      if (order[vertex] >= max(c(order[parents], -Inf))) {
        # Checks if parents are before or same
        if (c.fcn == "none") {
          # Calculate score in R
          if (.format == "raw") {
            # calculate score from raw data matrix
            # Response vector for linear regression
            Y <- pp.dat$data[pp.dat$non.int[[vertex]], vertex]
            sigma2 <- sum(Y^2)

            if (length(parents) + pp.dat$intercept != 0) {
              # Get data matrix on which linear regression is based
              Z <- pp.dat$data[pp.dat$non.int[[vertex]], parents, drop = FALSE]
              if (pp.dat$intercept) {
                Z <- cbind(1, Z)
              }

              # Calculate the scaled error covariance using QR decomposition
              Q <- qr.Q(qr(Z))
              sigma2 <- sigma2 - sum((Y %*% Q)^2)
            }
          } else if (.format == "scatter") {
            # Calculate the score based on pre-calculated scatter matrices
            # If an intercept is allowed, add a fake parent node
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

          # Return local score
          lscore <- -0.5 * pp.dat$data.count[vertex] * (1 + log(sigma2 / pp.dat$data.count[vertex])) -
            pp.dat$lambda * (1 + length(parents))
          return(lscore)
        } else {
          # Calculate score with the C++ library
          .Call(localScore, c.fcn, pp.dat, vertex, parents, c.fcn.options(...))
        }
      } else {
        # set score to minus infinity if vertex earlier than parents
        skip <- -Inf
        return(skip)
      }
    }
  ),
  inheritPackage = TRUE
)
