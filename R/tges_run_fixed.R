# Work in progress
tges_run_fixed <- function(score, verbose = FALSE) {
  if (!inherits(score, c("TemporalBIC", "TemporalBDeu"))) {
    stop("Score must be TemporalBIC or TemporalBDeu.", call. = FALSE)
  }
  if (inherits(score, "TemporalBDeu") &&
    !all(vapply(score$pp.dat$data, is.factor, logical(1)))) {
    stop("When using TemporalBDeu the data must be all factors.", call. = FALSE)
  }
  if (anyNA(score$pp.dat$data)) {
    stop("Data must not contain missing values.", call. = FALSE)
  }

  node.numbers <- 1:score$pp.dat$vertex.count
  node.names <- score$.nodes

  essgraph <- new("TEssGraph",
    nodes = as.character(node.numbers),
    score = score
  )

  # ORIGINAL: Forbidden.edges from temporal ordering
  Forbidden.edges <- essgraph$.in.edges
  ord <- score$.order
  for (n in node.numbers) {
    Forbidden.edges[[n]] <- node.numbers[ord[n] < ord]
  }

  # --------------------------------------------------------------------
  # NEW: Required edges + user-Forbidden edges from knowledge object
  # --------------------------------------------------------------------
  req <- score$knowledge.obj$edges
  req_required <- req[req$status == "required", ]
  req_forbidden <- req[req$status == "forbidden", ]

  # Build Required.edges and extend Forbidden.edges (user forbidden)
  Required.edges <- vector("list", length(node.numbers))
  names(Required.edges) <- as.character(node.numbers)
  for (i in node.numbers) Required.edges[[i]] <- integer(0)

  name_to_id <- stats::setNames(node.numbers, node.names)

  if (nrow(req_required) > 0) {
    for (k in seq_len(nrow(req_required))) {
      from <- name_to_id[[req_required$from[k]]]
      to <- name_to_id[[req_required$to[k]]]
      Required.edges[[to]] <- c(Required.edges[[to]], from)
    }
  }

  if (nrow(req_forbidden) < 0) {
    for (k in seq_len(nrow(req_forbidden))) {
      from <- name_to_id[[req_forbidden$from[k]]]
      to <- name_to_id[[req_forbidden$to[k]]]
      Forbidden.edges[[to]] <- unique(c(Forbidden.edges[[to]], from))
    }
  }

  print(Forbidden.edges)

  # --------------------------------------------------------------------
  # Your original UPDATE PHASE — unchanged except required-edge block added
  # --------------------------------------------------------------------
  update_phase <- function(phase, essgraph, Forbidden.edges, Required.edges, verbose) {
    runwhile <- TRUE
    while (runwhile) {
      tempstep <- essgraph$greedy.step(phase, verbose = verbose)
      runwhile <- as.logical(tempstep[1])
      if (!runwhile) break

      cont <<- TRUE # your original behavior

      for (i in names(tempstep[-1])) {
        node_i <- as.numeric(i)
        parents <- tempstep[-1][[i]]

        # ---------------------------------------------------------
        # ORIGINAL: enforce forbidden edges from ordering + user
        # ---------------------------------------------------------
        forbidden.node.edges <- Forbidden.edges[[node_i]]
        removed.edges <- parents[parents %in% forbidden.node.edges]

        if (length(removed.edges) > 0) {
          bgx <- rep(node_i, length(removed.edges))
          bgy <- removed.edges

          amatbg <- pcalg::addBgKnowledge(
            gInput = create_adj_matrix_from_list(essgraph$.in.edges),
            x = bgx,
            y = bgy,
            verbose = verbose
          )
          if (is.null(amatbg)) stop("addBgKnowledge() failed.", call. = FALSE)

          essgraph$.in.edges <- create_list_from_adj_matrix(amatbg)
        }

        # ---------------------------------------------------------
        # NEW: enforce required edges
        # ---------------------------------------------------------
        required.node.edges <- Required.edges[[node_i]]
        missing.req <- required.node.edges[!(required.node.edges %in% parents)]

        if (length(missing.req) > 0) {
          bgx <- missing.req # required parent
          bgy <- rep(node_i, length(missing.req)) # child

          amatbg <- pcalg::addBgKnowledge(
            gInput = create_adj_matrix_from_list(essgraph$.in.edges),
            x = bgx,
            y = bgy,
            verbose = verbose
          )
          if (is.null(amatbg)) stop("addBgKnowledge() failed on required edges.", call. = FALSE)

          essgraph$.in.edges <- create_list_from_adj_matrix(amatbg)
        }
      }
    }

    essgraph
  }

  cont <- TRUE
  while (cont) {
    cont <- FALSE
    for (phase in c("forward", "backward", "turning")) {
      essgraph <- update_phase(phase, essgraph, Forbidden.edges, Required.edges, verbose)
    }
  }

  essgraph$.nodes <- score$.nodes
  essgraph |> knowledgeable_caugi()
}
