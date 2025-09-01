# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Public API  ──────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Perform causal discovery using the temporal FCI algorithm (TFCI)
#'
#' Use a modification of the FCI algorithm that makes use of background knowledge
#' in the format of a partial ordering. This may, for instance, come about when
#' variables can be assigned to distinct tiers or periods (i.e., a temporal ordering).
#'
#' @details
#' The temporal/tiered background information enters several places in the TFCI
#' algorithm: (1) In the skeleton construction phase, when looking for separating
#' sets \eqn{Z} between two variables \eqn{X} and \eqn{Y}, \eqn{Z} is not allowed to
#' contain variables that are strictly after both \eqn{X} and \eqn{Y} in the temporal
#' order (as specified by the \code{knowledge} tiers). (2) This also applies to the
#' subsequent phase where the algorithm searches for possible D-SEP sets. (3) Prior
#' to other orientation steps, any cross-tier edges get an arrowhead placed at their
#' latest node.
#'
#' After this, the usual FCI orientation rules are applied; see \link[pcalg]{udag2pag}
#' for details.
#'
#' @inheritParams tpc
#' @param knowledge A \emph{knowledge} object describing tiers/periods and optional
#'   forbidden/required edges. This replaces the legacy \code{order} interface and
#'   is the preferred way to supply temporal background knowledge.
#' @param order \strong{Deprecated}. A character vector with period-prefixes in their
#'   temporal order (e.g., \code{c("p1", "p2")}). If supplied (and \code{knowledge}
#'   is \code{NULL}), a temporary \code{knowledge} object is constructed using
#'   [tidyselect::starts_with] for each prefix. Supplying
#'   both \code{knowledge} and \code{order} is an error.
#' @param methodOri Method for handling conflicting separating sets when orienting
#'   edges; must be one of \code{"standard"}, \code{"conservative"} (the default) or
#'   \code{"maj.rule"}. See \link[pcalg]{pc} for further details.
#'
#' @return A \code{discography} tibble with columns \code{from}, \code{to}, and
#'   \code{edge_type}. The underlying orientation corresponds to a PAG learned by
#'   TFCI under the supplied temporal background knowledge.
#'
#' @examples
#' # simulate linear Gaussian data with an unobserved variable L1
#' set.seed(123)
#' n <- 100
#' L1 <- rnorm(n)
#' X1 <- rnorm(n)
#' X2 <- L1 + X1 + rnorm(n)
#' X3 <- X1 + rnorm(n)
#' X4 <- X3 + L1 + rnorm(n)
#' d <- data.frame(
#'   p1_X1 = X1,
#'   p1_X2 = X2,
#'   p2_X3 = X3,
#'   p2_X4 = X4
#' )
#'
#' # Build knowledge from tidy tier rules (recommended)
#' kn <- knowledge(
#'   d,
#'   tier(
#'     p1 ~ tidyselect::starts_with("p1_"),
#'     p2 ~ tidyselect::starts_with("p2_")
#'   )
#' )
#'
#' # Use TFCI to recover a PAG (returned as a discography tibble)
#' tfci(d, test = corTest, knowledge = kn)
#'
#' # Legacy interface (deprecated): supply order only
#' \dontrun{
#' tfci(d, test = corTest, order = c("p1", "p2"))
#' }
#'
#' @include tpc.R
#' @importClassesFrom pcalg pcAlgo
#' @export
tfci <- function(data = NULL,
                 knowledge = NULL,
                 order = NULL,
                 alpha = 10^(-1),
                 test = regTest,
                 suffStat = NULL,
                 method = "stable.fast",
                 methodNA = "none",
                 methodOri = "conservative",
                 directed_as_undirected = FALSE,
                 varnames = NULL,
                 ...) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "methods", "pcalg", "stats", "tidyselect"
    ),
    function_name = "tfci"
  )

  if (!(methodNA %in% c("none", "cc", "twd"))) {
    stop("Invalid choice of method for handling NA values.")
  }
  if (is.null(data) && is.null(suffStat)) {
    stop("Either data or sufficient statistic must be supplied.")
  }
  if (!(methodOri %in% c("standard", "conservative", "maj.rule"))) {
    stop("Orientation method must be one of standard, conservative or maj.rule.")
  }

  if (!is.null(knowledge) && !is.null(order)) {
    stop(
      "Both `knowledge` and `order` supplied. ",
      "Please supply a knowledge object."
    )
  }

  # legacy order support (deprecated)
  if (is.null(knowledge) && !is.null(order)) {
    warning(
      "`order` is deprecated in version 1.0.0 and will be removed in a ",
      "future version. Please supply a `knowledge` object instead."
    )
    vnames0 <- if (is.null(data)) varnames else names(data)
    knowledge <- .build_knowledge_from_order(order, data = data, vnames = vnames0)
  }

  is_knowledge(knowledge)

  # NA handling
  if (!is.null(data) && any(is.na(data))) {
    if (methodNA == "none") {
      stop("Inputted data contain NA values, but no method for handling missing NAs was supplied.")
    } else if (methodNA == "cc") {
      data <- stats::na.omit(data)
      if (nrow(data) == 0) {
        stop("Complete case analysis chosen, but inputted data contain no complete cases.")
      }
    }
  }

  # variable names
  vnames <- if (is.null(data)) varnames else names(data)
  if (is.null(vnames) || !length(vnames)) {
    stop("Could not determine variable names. Supply `data` or `varnames`.")
  }

  # ensure all vars appear in knowledge
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

  # CI test that forbids conditioning on future tiers
  indep_test_dir <- dirTest(test, vnames, knowledge)

  # sufficient statistics (built-ins or user-supplied)
  if (is.null(suffStat)) {
    thisTestName <- deparse(substitute(test))
    if (thisTestName == "regTest") {
      thisSuffStat <- make_suff_stat(data, type = "regTest")
    } else if (thisTestName == "corTest") {
      thisSuffStat <- make_suff_stat(data, type = "corTest")
    } else {
      stop("suffStat needs to be supplied when using a non-builtin test.")
    }
  } else {
    thisSuffStat <- suffStat
    methodNA <- "none"
  }

  # pcalg background constraints (forbidden/required) from knowledge
  constraints <- .pcalg_constraints_from_knowledge(knowledge,
    labels = vnames,
    directed_as_undirected = directed_as_undirected
  )

  # learn skeleton
  skel <- pcalg::skeleton(
    suffStat = thisSuffStat,
    indepTest = indep_test_dir,
    alpha = alpha,
    labels = vnames,
    method = method,
    fixedGaps = constraints$fixedGaps,
    fixedEdges = constraints$fixedEdges,
    ...
  )
  ntests <- sum(skel@n.edgetests)

  # pdsep step: expand to MAG/PAG skeleton
  nvar <- length(skel@graph@nodes)
  fci_skel <- pcalg::pdsep(
    skel = skel,
    suffStat = thisSuffStat,
    indepTest = indep_test_dir,
    p = nvar,
    sepset = skel@sepset,
    pMax = skel@pMax,
    unfVect = c(),
    alpha = alpha
  )
  nextratests <- fci_skel$n.edgetests
  ntests <- ntests + nextratests

  # optional conservative / majority-rule orientation info
  conservative <- identical(methodOri, "conservative")
  maj.rule <- identical(methodOri, "maj.rule")
  unfVect <- NULL

  if (conservative || maj.rule) {
    tmp <- methods::new("pcAlgo",
      graph = as.graphNEL(t(fci_skel$G)),
      call = skel@call,
      n = integer(0), max.ord = as.integer(fci_skel$max.ord),
      n.edgetests = nextratests,
      sepset = fci_skel$sepset,
      pMax = fci_skel$pMax,
      zMin = matrix(NA, 1, 1)
    )
    tmpres <- pcalg::pc.cons.intern(
      tmp,
      suffStat = thisSuffStat,
      indepTest = indep_test_dir,
      alpha = alpha,
      version.unf = c(1, 1),
      maj.rule = maj.rule
    )
    unfVect <- tmpres$unfTripl
    fci_skel$sepset <- tmpres$sk@sepset
  }

  # orient into a PAG using knowledge tiers
  res <- tpag(fci_skel, knowledge = knowledge, unfVect = unfVect)

  # pack up tpag result
  out <- tamat(amat = res, order = knowledge$tiers$label, type = "ag") |>
    discography()
  out
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Helpers  ────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Order-restrict a PAG skeleton by temporal knowledge
#'
#' Given a PAG adjacency matrix (with entries \code{0} = none, \code{1} = circle,
#' \code{2} = arrowhead, \code{3} = tail; indexed as \code{[from, to]} in the pcalg
#' convention), enforce temporal constraints implied by \code{knowledge}: whenever
#' an edge crosses tiers, place an arrowhead at the later node.
#'
#' @param amat A square numeric adjacency matrix in pcalg PAG format
#'   (rows/columns named by variable names).
#' @param knowledge A \emph{knowledge} object that provides tier labels for variables.
#'
#' @return The modified adjacency matrix with arrowheads added at the later node
#'   for cross-tier pairs.
#' @keywords internal
order_restrict_pag_skel <- function(amat, knowledge) {
  p <- nrow(amat)
  vnames <- rownames(amat)

  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (amat[j, i] != 0 && is_after(vnames[i], vnames[j], knowledge)) {
        # place an arrowhead at the later node
        amat[j, i] <- 2
        # note: [i,j] stays as-is (typically 1: circle) in the skeleton
      }
    }
  }
  amat
}

#' Remove illegal separating sets under temporal knowledge
#'
#' Given a \code{sepset} structure (as used by \pkg{pcalg}) and temporal background
#' knowledge, drop any separating set that conditions on a variable strictly after
#' both endpoints.
#'
#' @param sepset A nested list of separating sets, typically \code{skel@sepset}
#'   from \pkg{pcalg}, where \code{sepset[[i]][[j]]} is a vector of node indices
#'   that separate node \code{i} and node \code{j}, or \code{NULL}.
#' @param knowledge A \emph{knowledge} object that provides tier labels for variables.
#' @param vnames Character vector of variable names, used to translate indices in
#'   \code{sepset} into names for tier comparison.
#'
#' @return The input \code{sepset} with any disallowed separating sets replaced by
#'   \code{NULL}. Emits a warning each time such a set is removed.
#' @keywords internal
order_restrict_sepset <- function(sepset, knowledge, vnames) {
  p <- length(vnames)

  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      thisSS <- sepset[[i]][[j]]
      if (length(thisSS) > 0) {
        for (k in seq_along(thisSS)) {
          if (is_after(vnames[thisSS[k]], vnames[i], knowledge) &&
            is_after(vnames[thisSS[k]], vnames[j], knowledge)) {
            sepset[[i]][[j]] <- NULL
            warning("Found sepset that was not allowed due to temporal order!")
            break
          }
        }
      }
    }
  }
  sepset
}

#' Orient a PAG from a skeleton using temporal knowledge
#'
#' Apply temporal restrictions and standard FCI orientation rules to produce a PAG.
#' Cross-tier edges receive an arrowhead at the later node and separating sets that
#' condition on the future are dropped (if \code{cautious = TRUE}) before calling
#' \code{\link[pcalg]{udag2pag}}.
#'
#' @param skel A skeleton-like object as returned by \code{\link[pcalg]{pdsep}},
#'   containing \code{$G} (adjacency), \code{$sepset}, \code{$pMax}, and
#'   \code{$max.ord}.
#' @param knowledge A \emph{knowledge} object that provides tier labels for variables.
#' @param unfVect Optional vector of unfaithful triples from conservative/majority-rule
#'   orientation (see \pkg{pcalg}); may be \code{NULL}.
#' @param cautious Logical; if \code{TRUE}, remove any separating set that violates
#'   temporal constraints before orientation.
#'
#' @return A PAG adjacency matrix in pcalg format (integer codes \code{0/1/2/3}).
#' @keywords internal
tpag <- function(skel, knowledge, unfVect, cautious = TRUE) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "pcalg"
    ),
    function_name = "tpag"
  )

  # boolean amat -> add 0 converts to numeric
  amat <- order_restrict_pag_skel(skel$G + 0, knowledge = knowledge)
  sepsets <- skel$sepset

  # orientation rules to use (skip 5–7: selection bias rules)
  userules <- rep(TRUE, 10)
  userules[5:7] <- FALSE

  if (cautious) {
    sepsets <- order_restrict_sepset(
      sepsets,
      knowledge = knowledge,
      vnames = rownames(skel$G)
    )
  }

  pcalg::udag2pag(
    amat,
    sepset = sepsets,
    rules = userules,
    unfVect = unfVect
  )
}
