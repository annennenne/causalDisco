# ──────────────────────────────────────────────────────────────────────────────
# ───────────── Conversion to External Engines: Tetrad, pcalg, bnlearn ─────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Convert to Tetrad `edu.cmu.tetrad.data.Knowledge`
#'
#' @description
#' Converts a `knowledge` object to a Tetrad `edu.cmu.tetrad.data.Knowledge`.
#' This requires `rJava`. This is used internally, when setting knowledge with
#' `set_knowledge` for methods using the Tetrad engine. `set_knowledge` is used
#' internally, when using the `disco` function with knowledge given.
#' @param kn A `knowledge` object.
#'
#' @returns A Java `edu.cmu.tetrad.data.Knowledge` object.
#'
#' @example inst/roxygen-examples/as_tetrad_knowledge-example.R
#'
#' @family knowledge functions
#' @concept knowledge
#'
#' @export
as_tetrad_knowledge <- function(kn) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "purrr",
      "rJava"
    ),
    function_name = "as_tetrad_knowledge"
  )

  is_knowledge(kn)
  # nocov start
  if (!rJava::.jniInitialized) {
    init_java()
  } # nocov end

  j <- rJava::.jnew("edu/cmu/tetrad/data/Knowledge")

  # attach every variable that has a tier label
  purrr::pwalk(
    list(kn$vars$var, kn$vars$tier),
    function(v, t) {
      if (!is.na(t)) {
        idx <- match(t, kn$tiers$label) # row position = tier rank
        j$addToTier(as.integer(idx), v)
      }
    }
  )

  # transfer forbidden / required edges
  purrr::pwalk(
    kn$edges,
    function(status, from, to, ...) {
      switch(
        status,
        forbidden = j$setForbidden(from, to),
        required = j$setRequired(from, to)
      )
    }
  )

  j
}

#' Convert background knowledge to pcalg constraint matrices
#'
#' \pkg{pcalg} only supports _undirected_ (symmetric) background constraints:
#' * **fixed_gaps**  - forbidding edges (zeros enforced)
#' * **fixed_edges** - requiring edges (ones enforced)
#'
#' This function takes a \code{knowledge} object (with only forbidden/required
#' edges, no tiers) and returns the two logical matrices in the exact
#' variable order you supply.
#'
#' @param kn A \code{knowledge} object.  Must have no tier information.
#' @param labels Character vector of all variable names, in the exact order
#'   of your data columns.  Every variable referenced by an edge in \code{kn}
#'   must appear here.
#' @param directed_as_undirected Logical (default \code{FALSE}).  If
#'   \code{FALSE}, we require that every edge in \code{kn} has its
#'   mirror-image present as well, and will error if any are missing.  If
#'   \code{TRUE}, we automatically mirror every directed edge into
#'   an undirected constraint.
#'
#' @returns A list with two elements, each an \code{n × n} logical matrix
#' corresponding to \pkg{pcalg} `fixed_gaps` and `fixed_edges` arguments.
#'
#' @section Errors:
#' * If the knowledge object contains tiered knowledge.
#' * If \code{directed_as_undirected = FALSE} and any edge lacks its
#'   symmetrical counterpart. This can only hold for forbidden edges.
#'
#' @example inst/roxygen-examples/as_pcalg_constraints-example.R
#'
#' @family knowledge functions
#' @concept knowledge
#'
#' @export
as_pcalg_constraints <- function(
  kn,
  labels = kn$vars$var,
  directed_as_undirected = FALSE
) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "dplyr",
      "pcalg",
      "rlang"
    ),
    function_name = "as_pcalg_constraints"
  )

  is_knowledge(kn)

  if (any(!is.na(kn$vars$tier))) {
    stop(
      "Tiered background knowledge cannot be utilised by the pcalg engine.\n",
      "pcalg does not support directed tier constraints."
    )
  }
  if (!is.character(labels) || length(labels) == 0L) {
    stop("`labels` must be a non-empty character vector.", call. = FALSE)
  }
  if (length(labels) != length(unique(labels))) {
    stop("`labels` must be unique.", call. = FALSE)
  }
  # check that labels and knowledge object are aligned
  if (!setequal(labels, kn$vars$var)) {
    # all labels that aren't in knowledge
    bad_vars <- setdiff(labels, kn$vars$var)
    if (length(bad_vars)) {
      stop(
        "`labels` contained variables that were not in the knowledge object: [",
        paste(bad_vars, collapse = ", "),
        "]",
        call. = FALSE
      )
    }
    # all vars that aren't in labels
    missing_vars <- setdiff(kn$vars$var, labels)
    if (length(missing_vars)) {
      stop(
        "`labels` must contain all variables in the knowledge",
        " object. The following is missing: [",
        paste(missing_vars, collapse = ", "),
        "]\nYou can add variables to your knowledge object with add_vars().",
        call. = FALSE
      )
    } else {
      # nocov start
      # this is a future-proofing security measure, not reachable as of now
      stop(
        "`labels` must contain all variables in the knowledge object.",
        call. = FALSE
      )
    }
    # nocov end
  }

  p <- length(labels)
  fixed_gaps <- matrix(FALSE, p, p, dimnames = list(labels, labels))
  fixed_edges <- matrix(FALSE, p, p, dimnames = list(labels, labels))
  idx <- rlang::set_names(seq_along(labels), labels)

  if (!directed_as_undirected) {
    bad <- kn$edges |>
      dplyr::anti_join(kn$edges, by = c("from" = "to", "to" = "from")) |>
      dplyr::mutate(desc = paste0(from, " --> ", to)) |>
      dplyr::pull(desc)
    if (length(bad)) {
      stop(
        "pcalg does not support asymmetric edges.\n",
        "The following have no symmetrical counterpart:\n  * ",
        paste(bad, collapse = "\n  * "),
        call. = FALSE
      )
    }
  }

  # fill forbidden
  forb <- dplyr::filter(kn$edges, status == "forbidden")
  for (k in seq_len(nrow(forb))) {
    i <- match(forb$from[k], labels, nomatch = NA_integer_)
    j <- match(forb$to[k], labels, nomatch = NA_integer_)
    # extra security measure
    if (is.na(i) || is.na(j)) {
      stop("Forbidden edge refers to unknown variable(s).", call. = FALSE)
    }
    fixed_gaps[i, j] <- TRUE
    if (directed_as_undirected) fixed_gaps[j, i] <- TRUE
  }

  # fill required
  req <- dplyr::filter(kn$edges, status == "required")
  for (k in seq_len(nrow(req))) {
    i <- match(req$from[k], labels, nomatch = NA_integer_)
    j <- match(req$to[k], labels, nomatch = NA_integer_)
    # extra security measure
    if (is.na(i) || is.na(j)) {
      stop("Forbidden edge refers to unknown variable(s).", call. = FALSE)
    }
    fixed_edges[i, j] <- TRUE
    if (directed_as_undirected) fixed_edges[j, i] <- TRUE
  }

  list(fixed_gaps = fixed_gaps, fixed_edges = fixed_edges)
}

#' Convert background knowledge to bnlearns white- and blacklists
#'
#' @description
#' Converts a `knowledge` object to a list of two data frames, namely
#' `whitelist` and `blacklist`, which can be used as arguments for
#' \pkg{bnlearn} algorithms. The `whitelist` contains all required edges, and the
#' `blacklist` contains all forbidden edges. Tiers will be made into forbidden
#' edges before running the conversion.
#'
#' @param kn A \code{knowledge} object.  Must have no tier information.
#'
#' @returns A list with two elements, `whitelist` and `blacklist`, each a data
#' frame containing the edges in a `from`, `to` format.
#'
#' @example inst/roxygen-examples/as_bnlearn_knowledge-example.R
#'
#' @family knowledge functions
#' @concept knowledge
#'
#' @export
as_bnlearn_knowledge <- function(kn) {
  .check_if_pkgs_are_installed(
    pkgs = c(
      "dplyr"
    ),
    function_name = "as_bnlearn_knowledge"
  )

  is_knowledge(kn)

  # whitelist holds all required edges in a "from", "to" dataframe
  whitelist <- dplyr::filter(kn$edges, status == "required") |>
    dplyr::select(from, to) |>
    as.data.frame()

  # blacklist holds all forbidden edges (including tier violations)
  blacklist <- forbid_tier_violations(kn)$edges |>
    dplyr::filter(status == "forbidden") |>
    dplyr::select(from, to) |>
    as.data.frame()

  list(
    whitelist = whitelist,
    blacklist = blacklist
  )
}


#' Convert background knowledge to caugi object
#'
#' @description
#' Converts a `knowledge` object to a `caugi` object used for plotting.
#'
#' @param kn A \code{knowledge} object.
#'
#' @returns A list with the `caugi` object alongside information.
#'
#' @examples
#' data(tpc_example)
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   ),
#'   child_x1 %-->% youth_x3,
#' )
#' cg <- knowledge_to_caugi(kn)
#'
#' @family knowledge functions
#' @concept knowledge
#'
#' @keywords internal
#' @noRd
knowledge_to_caugi <- function(kn) {
  .check_if_pkgs_are_installed(
    pkgs = c("dplyr", "caugi"),
    function_name = "knowledge_to_caugi"
  )
  is_knowledge(kn)

  ## ---- build caugi(vars...) ----
  caugi_call <- as.call(
    c(list(quote(caugi::caugi)), lapply(kn$vars$var, as.name))
  )

  ## ---- build edge calls ----
  edges <- kn$edges[kn$edges$status %in% c("required", "forbidden"), ]

  if (nrow(edges) == 0) {
    cg <- eval(caugi_call, envir = parent.frame())
  } else {
    edge_calls <- lapply(seq_len(nrow(edges)), function(i) {
      as.call(list(
        as.name("%-->%"),
        as.name(edges$from[i]),
        as.name(edges$to[i])
      ))
    })

    full_call <- as.call(
      c(list(quote(caugi::add_edges), caugi_call), edge_calls)
    )

    cg <- eval(full_call, envir = parent.frame())
  }

  ## ---- build tiers list ----
  if (all(is.na(kn$vars$tier))) {
    tiers <- list()
  } else {
    tier_levels <- unique(na.omit(kn$vars$tier))
    tiers <- lapply(tier_levels, function(t) kn$vars$var[kn$vars$tier == t])
    names(tiers) <- tier_levels
  }

  ## ---- return list ----
  list(
    caugi = cg,
    tiers = tiers
  )
}

#' Combine knowledge and caugi object
#' @param cg A `caugi` object.
#' @param kcg A `knowledgeable_caugi` object.
#' @returns A list with the updated `caugi` object alongside information.
#' @keywords internal
#' @noRd
combine_knowledge_and_caugi <- function(cg, kn) {
  .check_if_pkgs_are_installed(
    pkgs = c("dplyr", "caugi"),
    function_name = "combine_knowledge_and_caugi"
  )

  # Extract edges from knowledge
  edges <- kn$edges[kn$edges$status %in% c("required", "forbidden"), ]

  if (nrow(edges) == 0) {
    # No new edges; just return the original caugi
    combined_cg <- cg
  } else {
    # Convert each edge to expression: from %-->% to
    edge_calls <- lapply(seq_len(nrow(edges)), function(i) {
      as.call(list(
        as.name("%-->%"),
        as.name(edges$from[i]),
        as.name(edges$to[i])
      ))
    })

    # Combine with existing caugi
    full_call <- as.call(c(list(quote(caugi::add_edges), cg), edge_calls))

    combined_cg <- eval(full_call, envir = parent.frame())
  }

  # Build tiers from knowledge object
  if (all(is.na(kn$vars$tier))) {
    tiers <- setNames(list(kn$vars$var), "NA")
  } else {
    tier_levels <- unique(na.omit(kn$vars$tier))
    tiers <- lapply(tier_levels, function(t) kn$vars$var[kn$vars$tier == t])
    names(tiers) <- tier_levels
  }

  list(
    caugi = combined_cg,
    tiers = tiers
  )
}
