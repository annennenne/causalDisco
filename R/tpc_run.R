# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Public API  ──────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Causal Discovery Using the Temporal PC Algorithm (TPC)
#'
#' @description
#' Run a tier-aware variant of the PC algorithm that respects background
#' knowledge about a partial temporal order. Supply the temporal order via a
#' \code{knowledge} object. For backward compatibility, you may still pass
#' \code{order}, but that interface is deprecated and will be removed in a
#' future release.
#'
#' @param data A data frame with the observed variables. Columns are variables.
#'   When using the deprecated \code{order} argument, variables should be
#'   prefixed with their period name (see examples). When using
#'   \code{knowledge}, prefixes are not required.
#' @param knowledge A \code{knowledge} object created with \code{knowledge()},
#'   encoding tier assignments and optional forbidden/required edges. This is
#'   the preferred way to provide temporal background knowledge.
#' @param order A character vector of period prefixes in temporal order.
#'   Deprecated; use \code{knowledge} instead. If supplied, it is converted
#'   internally to tier knowledge using \code{tidyselect::starts_with()} for
#'   each prefix.
#' @param alpha The alpha level used as the per-test significance
#'   threshold for conditional independence testing.
#' @param test A conditional independence test. The default \code{reg_test}
#'   uses a regression-based information-loss test. Another available option is
#'   \code{cor_test} which tests for vanishing partial correlations. User-supplied
#'   functions may also be used; see details for the required interface.
#' @param suffStat A sufficient statistic. If supplied, it is passed directly
#'   to the test and no statistics are computed from \code{data}. Its structure
#'   depends on the chosen \code{test}.
#' @param method Skeleton construction method, one of \code{"stable"},
#'   \code{"original"}, or \code{"stable.fast"} (default). See
#'   \code{\link[pcalg]{skeleton}} for details.
#' @param na_method Handling of missing values, one of \code{"none"} (default;
#'   error on any \code{NA}), \code{"cc"} (complete-case analysis), or
#'   \code{"twd"} (test-wise deletion).
#' @param orientation_method Conflict-handling method when orienting edges.
#'   Currently only the conservative method is available.
#' @param output One of \code{"tpdag"}, \code{"tskeleton"}, \code{"pcAlgo"},
#'   or \code{"caugi"}. If \code{"tskeleton"}, return the temporal
#'   skeleton without directions. If \code{"tpdag"} (default), return a
#'   temporal partially directed acyclic graph (TPDAG). If \code{"pcAlgo"},
#'   return a \code{\link[pcalg]{pcAlgo-class}} object for compatibility with
#'   \pkg{pcalg}. If \code{"caugi"}, return a `caugi` and a `knowledge`
#'   (`knowledgeable_caugi`) object.
#' @param directed_as_undirected Logical; if \code{TRUE}, treat any directed
#'   edges in \code{knowledge} as undirected during skeleton learning. This
#'   is due to the fact that \pkg{pcalg} does not allow directed edges in
#'   \code{fixedEdges} or \code{fixedGaps}. Default is \code{FALSE}.
#' @param varnames Character vector of variable names. Only needed when
#'   \code{data} is not supplied and all information is passed via
#'   \code{suffStat}.
#' @param ... Additional arguments passed to
#'   \code{\link[pcalg]{skeleton}} during skeleton construction.
#'
#' @details
#' Any independence test implemented in \pkg{pcalg} may be used; see
#' \code{\link[pcalg]{pc}}. When \code{na_method = "twd"}, test-wise deletion is
#' performed: for \code{cor_test}, each pairwise correlation uses complete cases;
#' for \code{reg_test}, each conditional test performs its own deletion. If you
#' supply a user-defined \code{test}, you must also provide \code{suffStat}.
#'
#' Temporal or tiered knowledge enters in two places:
#' \itemize{
#'   \item during skeleton estimation, candidate conditioning sets are pruned so
#'   they do not contain variables that are strictly after both endpoints;
#'   \item during orientation, any cross-tier edge is restricted to point
#'   forward in time.
#' }
#' The \code{order} argument is deprecated. If provided, it is converted to a
#' \code{knowledge} object by assigning variables to tiers using
#' \code{tidyselect::starts_with()} for each prefix.
#'
#' @return
#' If \code{output = "tpdag"} or \code{"tskeleton"}, an S3 list with entries
#' \code{$tamat} (temporal adjacency matrix), \code{$psi} (alpha level),
#' and \code{$ntests} (number of tests run). If \code{output = "pcAlgo"}, a
#' \code{\link[pcalg]{pcAlgo-class}} object. If \code{output = "caugi"},
#' a `caugi` and a `knowledge` (`knowledgeable_caugi`) object.
#'
#' @example inst/roxygen-examples/tpc_example.R
#'
#' @export
tpc_run <- function(data = NULL,
                    knowledge = NULL,
                    order = NULL,
                    alpha = 10^(-1),
                    test = reg_test,
                    suffStat = NULL,
                    method = "stable.fast",
                    na_method = "none",
                    orientation_method = "conservative",
                    output = "caugi",
                    directed_as_undirected = FALSE,
                    varnames = NULL, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg", "stats", "tidyselect"
    ),
    function_name = "tpc"
  )

  if (!output %in% c("tpdag", "tskeleton", "pcAlgo", "caugi")) {
    stop("Output must be tpdag, tskeleton, pcAlgo, or caugi.")
  }
  if (!na_method %in% c("none", "cc", "twd")) {
    stop("Invalid choice of method for handling NA values.")
  }
  if (is.null(data) && is.null(suffStat)) {
    stop("Either data or sufficient statistic must be supplied.")
  }
  if (!is.null(knowledge) && !is.null(order)) {
    stop(
      "Both `knowledge` and `order` supplied. ",
      "Please supply a knowledge object."
    )
  }
  # backward compatibility: build knowledge from legacy `order` if needed
  if (is.null(knowledge) && !is.null(order)) {
    warning(
      "`order` is deprecated in version 1.0.0 and will be removed in a ",
      "future version. Please supply a `knowledge` object instead."
    )
    vnames0 <- if (is.null(data)) varnames else names(data)
    knowledge <- .build_knowledge_from_order(order, data = data, vnames = vnames0)
  }

  if (is.null(knowledge)) {
    knowledge <- knowledge()
  }

  is_knowledge(knowledge)

  if (!is.null(data) && any(is.na(data))) {
    if (na_method == "none") {
      stop("Inputted data contain NA values, but no method for handling missing NAs was supplied.")
    } else if (na_method == "cc") {
      data <- stats::na.omit(data)
      if (nrow(data) == 0) {
        stop("Complete case analysis chosen, but inputted data contain no complete cases.")
      }
    }
  }

  vnames <- if (is.null(data)) varnames else names(data)
  if (is.null(vnames) || !length(vnames)) {
    stop("Could not determine variable names. Supply `data` or `varnames`.")
  }

  missing_vars <- setdiff(vnames, knowledge$vars$var)
  if (length(missing_vars)) {
    knowledge <- add_vars(knowledge, missing_vars)
  }
  bad_vars <- setdiff(knowledge$vars$var, vnames)
  if (length(bad_vars)) {
    stop(
      "Knowledge contains variables not present in `data`: ",
      paste(bad_vars, collapse = ", ")
    )
  }

  indep_test_dir <- dir_test(test, vnames, knowledge)

  if (is.null(suffStat)) {
    this_test_name <- deparse(substitute(test))
    if (this_test_name == "reg_test") {
      this_suffStat <- make_suffStat(data, type = "reg_test")
    } else if (this_test_name == "cor_test") {
      this_suffStat <- make_suffStat(data, type = "cor_test")
    } else {
      stop("suffStat needs to be supplied when using a non-builtin test.")
    }
  } else {
    this_suffStat <- suffStat
    na_method <- "none"
  }

  constraints <- .pcalg_constraints_from_knowledge(
    knowledge,
    labels = vnames,
    directed_as_undirected = directed_as_undirected
  )

  skel <- pcalg::skeleton(
    suffStat = this_suffStat,
    indepTest = indep_test_dir,
    alpha = alpha,
    labels = vnames,
    method = method,
    fixedGaps = constraints$fixed_gaps,
    fixedEdges = constraints$fixed_edges,
    ...
  )
  ntests <- sum(skel@n.edgetests)

  if (output == "tskeleton") {
    out <- list(
      tamat = tamat(amat = graph_to_amat(skel), order = knowledge$tiers$label),
      psi = alpha,
      ntest = ntests
    )
    class(out) <- "tskeleton"
    return(out)
  }

  res <- tpdag(skel, knowledge = knowledge)

  if (output == "tpdag") {
    out <- list(
      tamat = tamat(amat = graph_to_amat(res, to_from = FALSE), order = knowledge$tiers$label),
      psi = alpha,
      ntests = ntests
    )
    class(out) <- "tpdag"
    out
  } else if (output == "pcAlgo") {
    res
  } else if (output == "caugi") {
    amat <- graph_to_amat(res, to_from = FALSE)
    amat <- methods::as(amat, "matrix")
    cg <- caugi::as_caugi(amat, collapse = TRUE, class = "PDAG")
    knowledgeable_caugi(cg, knowledge)
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Helpers  ────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Build tiered knowledge from legacy order prefixes
#'
#' @description
#' Helper that converts a character \code{order} of prefixes into a
#' \code{knowledge} object by creating one tier per prefix and assigning
#' variables with \code{tidyselect::starts_with("<prefix>")}.
#'
#' @param order Character vector of prefixes in temporal order.
#' @param data Optional data frame used to freeze the knowledge variable set.
#' @param vnames Optional character vector of variable names when \code{data}
#'   is not supplied.
#'
#' @example inst/roxygen-examples/dot-build_knowledge_from_order_example.R
#'
#' @return A \code{knowledge} object with tiers matching \code{order}.
#' @keywords internal
#' @noRd
.build_knowledge_from_order <- function(order, data = NULL, vnames = NULL) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "rlang", "tidyselect", "utils"
    ),
    function_name = ".build_knowledge_from_order"
  )

  stopifnot(is.character(order), length(order) > 0)

  # build tier specs like: "<lbl>" ~ starts_with("<lbl>")
  fmls <- lapply(order, function(lbl) {
    rlang::new_formula(
      lhs = rlang::expr(!!lbl),
      rhs = rlang::expr(tidyselect::starts_with(!!lbl)),
      env = rlang::empty_env()
    )
  })

  # if data is provided, delegate to knowledge() with tier() rules
  if (!is.null(data)) {
    return(rlang::inject(knowledge(data, tier(!!!fmls))))
  }

  # otherwise, build a bare knowledge object from variable names
  if (is.null(vnames) && is.null(data)) {
    stop("`data` is NULL, so `vnames` should be provided.")
  }

  kn <- knowledge() |> add_vars(vnames)

  # create tiers in declared order
  for (lbl in order) {
    if (nrow(kn$tiers) == 0L) {
      kn <- add_tier(kn, !!lbl)
    } else {
      last <- utils::tail(kn$tiers$label, 1)
      kn <- rlang::inject(add_tier(kn, !!lbl, after = !!last))
    }
  }

  # assign tiers by prefix match; do not overwrite earlier assignments
  for (lbl in order) {
    hits <- startsWith(vnames, lbl)
    if (any(hits)) {
      idx <- match(vnames[hits], kn$vars$var)
      unassigned <- is.na(kn$vars$tier[idx])
      if (any(unassigned)) {
        kn$vars$tier[idx[unassigned]] <- lbl
      }
    }
  }
  kn
}

#' Temporally orient unshielded colliders
#'
#' @description
#' Given a CPDAG adjacency matrix and separation sets, orient v-structures
#' that do not contradict the current directions, respecting temporal tiering.
#'
#' @param amat Square adjacency matrix (from-to convention).
#' @param sepsets Separation sets as computed by \pkg{pcalg}.
#'
#' @example inst/roxygen-examples/v_orient_temporal_example.R
#'
#' @return The updated adjacency matrix with additional arrowheads.
#' @keywords internal
#' @noRd
v_orient_temporal <- function(amat, sepsets) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "gtools"
    ),
    function_name = "v_orient_temporal"
  )

  vnames <- rownames(amat) # TODO: not used
  nvar <- nrow(amat)

  for (i in 1:nvar) {
    theseAdj <- find_adjacencies(amat, i)

    # if there are at least two adjacent nodes
    if (length(theseAdj) >= 2) {
      adjpairs <- gtools::combinations(length(theseAdj), 2, v = theseAdj)

      npairs <- nrow(adjpairs)

      if (npairs >= 1) {
        for (j in 1:npairs) {
          thisPair <- adjpairs[j, ]
          j1 <- thisPair[1]
          j2 <- thisPair[2]
          thisPairAdj <- j2 %in% find_adjacencies(amat, j1)

          # if pair is not adjacent (unmarried)
          if (!thisPairAdj) {
            sepset1 <- sepsets[[j1]][[j2]]
            sepset2 <- sepsets[[j2]][[j1]]

            # if middle node is not a separator of two other nodes
            if (!(i %in% sepset1) && !(i %in% sepset2)) {
              # if this does not contradict directional information
              # already in the graph
              if (amat[i, j1] == 1 && amat[i, j2] == 1) {
                amat[j1, i] <- 0
                amat[j2, i] <- 0
              }
            }
          }
        }
      }
    }
  }
  amat
}

#' Find adjacencies of a node in an adjacency matrix
#'
#' @param amatrix Square adjacency matrix.
#' @param index Integer index of the node.
#'
#' @example inst/roxygen-examples/find_adjacencies_example.R
#'
#' @return Integer vector of adjacent node indices.
#' @keywords internal
#' @noRd
find_adjacencies <- function(amatrix, index) {
  union(
    which(as.logical(amatrix[index, ])),
    which(as.logical(amatrix[, index]))
  )
}

#' Compute tier indices for variables
#'
#' @description
#' Map variable names to their tier ranks according to a \code{knowledge}
#' object. Variables without a tier receive \code{NA}.
#'
#' @param kn A \code{knowledge} object.
#' @param vnames Character vector of variable names.
#'
#' @example inst/roxygen-examples/dot-tier_index_example.R
#'
#' @return Named integer vector of the same length as \code{vnames}.
#' @keywords internal
#' @noRd
.tier_index <- function(kn, vnames) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "stats"
    ),
    function_name = ".tier_index"
  )

  is_knowledge(kn)
  idx <- match(vnames, kn$vars$var)
  tiers <- kn$vars$tier[idx]
  rank <- match(tiers, kn$tiers$label)
  stats::setNames(rank, vnames)
}

#' Check whether one variable is strictly after another in tier order
#'
#' @param x,y Variable names.
#' @param knowledge A \code{knowledge} object.
#'
#' @example inst/roxygen-examples/is_after_example.R
#'
#' @return Logical. \code{TRUE} if \code{x} is in a strictly later tier than
#' \code{y}. Returns \code{FALSE} if either variable lacks a tier.
#'
#' @keywords internal
#' @noRd
is_after <- function(x, y, knowledge) {
  ti <- .tier_index(knowledge, c(x, y))
  if (any(is.na(ti))) {
    return(FALSE)
  }
  ti[[1]] > ti[[2]]
}

#' Directed indepTest wrapper that forbids conditioning on the future
#'
#' @description
#' Wrap a conditional independence test so that conditioning sets that are
#' strictly after both endpoints are rejected, implementing the temporal
#' restriction during skeleton learning.
#'
#' @param test A function \code{f(x, y, S, suffStat)} returning a p-value or
#'   test statistic compatible with \pkg{pcalg}.
#' @param vnames Character vector of variable names (labels).
#' @param knowledge A \code{knowledge} object.
#'
#' @example inst/roxygen-examples/dir_test_example.R
#'
#' @return A function with the same interface as \code{test}.
#' @keywords internal
#' @noRd
dir_test <- function(test, vnames, knowledge) {
  function(x, y, S, suffStat) {
    snames <- vnames[S]
    xname <- vnames[x]
    yname <- vnames[y]

    if (length(snames)) {
      for (s in snames) {
        if (isTRUE(is_after(s, xname, knowledge)) &&
          isTRUE(is_after(s, yname, knowledge))) {
          return(0)
        }
      }
    }
    do.call(test, list(x = x, y = y, S = S, suffStat = suffStat))
  }
}

#' Convert knowledge to \pkg{pcalg} constraints
#'
#' @description
#' Turn directed forbidden/required edges into undirected \code{fixedGaps} and
#' \code{fixedEdges} matrices in the supplied \code{labels} order. Tier
#' annotations are ignored here; use \code{order_restrict_amat_cpdag()} for
#' tier-based pruning.
#'
#' @param kn A \code{knowledge} object.
#' @param labels Character vector of variable names in the desired order.
#'
#' @example inst/roxygen-examples/dot-pcalg_constraints_from_knowledge_example.R
#'
#' @return A list with logical matrices \code{fixedGaps} and \code{fixedEdges}.
#' @keywords internal
#' @noRd
.pcalg_constraints_from_knowledge <- function(kn,
                                              labels,
                                              directed_as_undirected) {
  kn_undirected <- kn
  kn_undirected$vars$tier <- NA_character_
  as_pcalg_constraints(
    kn_undirected,
    labels = labels,
    directed_as_undirected = TRUE
  )
}

#' Remove disallowed backward edges across tiers
#'
#' @description
#' Apply tier constraints to a CPDAG adjacency matrix by zeroing any entry that
#' points from a later tier into an earlier tier.
#'
#' @param amat Square adjacency matrix (from-to convention).
#' @param knowledge A \code{knowledge} object with tier labels.
#'
#' @example inst/roxygen-examples/order_restrict_amat_cpdag_example.R
#'
#' @return The pruned adjacency matrix.
#' @keywords internal
#' @noRd
order_restrict_amat_cpdag <- function(amat, knowledge) {
  p <- nrow(amat)
  vnames <- rownames(amat)
  tr <- .tier_index(knowledge, vnames)

  if (all(is.na(tr))) {
    return(amat)
  }

  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (!is.na(tr[i]) && !is.na(tr[j]) && tr[i] > tr[j]) {
        amat[j, i] <- 0
      }
    }
  }
  amat
}

#' Orient a CPDAG under temporal background knowledge
#'
#' @description
#' Take a learned skeleton and apply tier-based pruning and v-structure
#' orientation, then delegate to \code{pcalg::addBgKnowledge()} for final
#' orientation under background knowledge.
#'
#' @param skel A \code{\link[pcalg]{pcAlgo-class}} skeleton result.
#' @param knowledge A \code{knowledge} object with tiers (and optionally edges).
#'
#' @example inst/roxygen-examples/tpdag_example.R
#'
#' @return A \code{\link[pcalg]{pcAlgo-class}} object with an oriented graph.
#' @keywords internal
#' @noRd
tpdag <- function(skel, knowledge) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "tpdag"
  )

  thisAmat <- graph_to_amat(skel)
  tempSkelAmat <- order_restrict_amat_cpdag(thisAmat, knowledge = knowledge)
  pcalg::addBgKnowledge(
    v_orient_temporal(tempSkelAmat, skel@sepset),
    checkInput = FALSE
  )
}

#' Construct sufficient statistics for built-in CI tests
#'
#' @description
#' Build the \emph{sufficient statistic} object expected by the built-in
#' conditional independence tests. Supports:
#' \itemize{
#'   \item \code{type = "reg_test"} — returns the original \code{data} and a
#'         logical indicator of which variables are binary;
#'   \item \code{type = "cor_test"} — returns a pairwise-complete correlation
#'         matrix and the sample size \code{n}.
#' }
#'
#' @param data A data frame (or numeric matrix) of variables used by the test.
#'   Columns are variables; rows are observations.
#' @param type A string selecting the test family. Must be either
#'   \code{"reg_test"} or \code{"cor_test"}.
#' @param ... currently ignored.
#'
#' @details
#' For \code{type = "reg_test"}, the return value is a list with elements:
#' \itemize{
#'   \item \code{data} — the original \code{data} object;
#'   \item \code{binary} — a logical vector (one per column) indicating whether
#'         the variable is binary.
#' }
#'
#' For \code{type = "cor_test"}, the return value is a list with elements:
#' \itemize{
#'   \item \code{C} — the correlation matrix computed with
#'         \code{use = "pairwise.complete.obs"};
#'   \item \code{n} — the number of rows in \code{data}.
#' }
#'
#' Any other \code{type} results in an error.
#'
#' @example inst/roxygen-examples/make_suffStat_example.R
#'
#' @return A list whose structure depends on \code{type}, suitable for passing
#'   as \code{suffStat} to the corresponding test.
#'
#' @keywords internal
#' @noRd
make_suffStat <- function(data, type, ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "stats"
    ),
    function_name = "make_suffStat"
  )

  if (type == "reg_test") {
    bin <- unlist(sapply(
      data,
      function(x) length(unique(stats::na.omit(x))) == 2
    ))
    suff <- list(data = data, binary = bin)
  } else if (type == "cor_test") {
    suff <- list(
      C = stats::cor(data, use = "pairwise.complete.obs"),
      n = nrow(data)
    )
  } else {
    stop(paste(
      type, "is not a supported type for autogenerating a sufficient statistic."
    ))
  }
  suff
}
