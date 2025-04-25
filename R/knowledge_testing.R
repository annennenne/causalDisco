# ---------------------------------------------------------------------------
# knowledge.R
#
# Public API
#   • knowledge()       – create an empty or seeded knowledge object.
#                         Can also be used as a mini-DSL:
#                         knowledge(data, tier(), forbidden(), required())
#   • add_vars()        – register variables
#   • add_tier()        – assign variables to tiers
#   • forbid_edge()     – store forbidden edges
#   • require_edge()    – store required edges
#   • knowledge()       –
#   • +.knowledge       – merge two knowledge objects
#   • as_tetrad()       – convert to Tetrad Knowledge object
#
# Representation
#   vars  : var, tier
#   edges : status, edge_type, from, to, tier_from, tier_to
# ---------------------------------------------------------------------------


#' Create a `knowledge` object
#'
#' @param vars Character vector of variable names.  Defaults to empty.
#'
#' @return An S3 object of class `"knowledge"`.
#' @keywords internal
.new_knowledge <- function(vars = character()) {
  stopifnot(is.character(vars), !anyDuplicated(vars))

  structure(
    list(
      vars = tibble::tibble(var = vars, tier = NA_integer_),
      edges = tibble::tibble(
        status     = character(),
        edge_type  = character(),
        from       = character(),
        to         = character(),
        tier_from  = integer(),
        tier_to    = integer()
      ),
      tier_labels = integer() # named int vector, e.g. c(Monday = 1L)
    ),
    class = "knowledge"
  )
}

#' Supported edge-type strings
#' @keywords internal
.allowed_edge_types <- c("directed", "undirected", "bidirected", "o->", "o-o", "<-o")

# ---------------------------------------------------------------------------
# internal helpers -----------------------------------------------------------

#' Resolve a tier label or number to an integer index (adds new labels)
#' @keywords internal
.resolve_tier <- function(.kn, tier) {
  ## ---------------- integer supplied -------------------------------------
  if (rlang::is_integerish(tier) && length(tier) == 1 && tier >= 1) {
    return(list(idx = as.integer(tier), kn = .kn))
  }

  ## ---------------- symbol / string supplied -----------------------------
  label <- rlang::as_string(tier)
  if (!nzchar(label)) {
    cli::cli_abort("Tier labels must be a non-empty symbol or string.")
  }

  if (label %in% names(.kn$tier_labels)) {
    return(list(idx = .kn$tier_labels[[label]], kn = .kn))
  }

  next_idx <- if (length(.kn$tier_labels)) max(.kn$tier_labels) + 1L else 1L
  .kn$tier_labels[[label]] <- next_idx
  list(idx = next_idx, kn = .kn)
}

#' Resolve a tidy-select or character spec to character names
#' @keywords internal
.resolved_vars <- function(.kn, spec) {
  lookup <- rlang::set_names(seq_along(.kn$vars$var), .kn$vars$var)

  tryCatch(
    names(tidyselect::eval_select(rlang::enquo(spec), lookup)),
    error = function(e) {
      out <- tryCatch(
        rlang::eval_tidy(rlang::enquo(spec)),
        error = function(...) NULL
      )
      if (is.character(out)) {
        return(out)
      }
      character(0)
    }
  )
}

#' Extract variable names from the RHS of a `tier()` formula
#' @keywords internal
.formula_vars <- function(rhs, .kn) {
  vars <- .resolved_vars(.kn, !!rhs)
  if (length(vars)) {
    return(vars)
  } # tidy-select succeeded
  unique(all.vars(rhs)) # fallback to plain symbols
}

#' Validate that no edge runs from higher tier to lower tier
#' @keywords internal
.validate_tier_rule <- function(edges_df) {
  tier_violations <- dplyr::filter(
    edges_df,
    !is.na(tier_from),
    !is.na(tier_to),
    tier_from > tier_to
  )
  if (nrow(tier_violations)) {
    stop("Edge(s) violate tier ordering: ",
      paste(tier_violations$from, "-->", tier_violations$to, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate that an edge is not simultaneously forbidden *and* required
#' @keywords internal
.validate_forbidden_required <- function(edges_df) {
  # look for groups where the same (from, to, edge_type) has both statuses
  clashes <- edges_df |>
    dplyr::group_by(edge_type, from, to) |>
    dplyr::filter(all(c("forbidden", "required") %in% status)) |>
    dplyr::ungroup() |>
    dplyr::distinct(edge_type, from, to) # one row per conflicting edge
  if (nrow(clashes)) {
    stop(
      "Edge(s) appear as both forbidden and required: ",
      paste0(clashes$from, " → ", clashes$to,
        " [", clashes$edge_type, "]",
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate that merging two knowledge objects introduces no tier-label conflict
#'
#' A conflict occurs when **two or more different labels map to the same
#' numeric tier index** after the merge (e.g. "January → 1" and "Monday → 1").
#' The function aborts with a clear message that lists every problematic
#' tier together with the colliding labels.
#'
#' @keywords internal
.validate_label_conflict <- function(tiers_left, tiers_right) {
  merged <- c(tiers_left, tiers_right)

  # If no labels are present, or if all labels are unique, return TRUE
  if (length(merged) < 2L) {
    return(invisible(TRUE))
  }

  conflicts <- Filter(
    function(lbl) length(unique(lbl)) > 1,
    split(names(merged), merged) # labels grouped by numeric tier
  )
  if (length(conflicts)) {
    msg <- vapply(conflicts,
      function(lbl) paste(unique(lbl), collapse = ", "),
      FUN.VALUE = character(1)
    )
    cli::cli_abort(
      "Conflicting tier labels detected:\n{.bullet {sprintf('tier %d: %s', as.integer(names(msg)), msg)}}"
    )
  }
  invisible(TRUE)
}


#' Add one or many edges to a knowledge object
#' @keywords internal
.add_edges <- function(.kn, status, edge_type, from, to) {
  # Reject unknown edge types
  if (!(edge_type %in% .allowed_edge_types)) {
    cli::cli_abort("edge_type must be one of {.val {paste(.allowed_edge_types, collapse = ', ')}}.")
  }

  # Resolve `from` / `to` specs into character vectors of variable names
  from_chr <- .resolved_vars(.kn, {{ from }})
  to_chr <- .resolved_vars(.kn, {{ to }})

  # Ensure all endpoint variables exist in `.kn$vars`
  .kn <- add_vars(.kn, unique(c(from_chr, to_chr)))

  # Cartesian product → one row per directed edge, then annotate
  block <- tidyr::crossing(from = from_chr, to = to_chr) |>
    dplyr::mutate(
      status    = status,
      edge_type = edge_type,
      tier_from = .kn$vars$tier[match(from, .kn$vars$var)],
      tier_to   = .kn$vars$tier[match(to, .kn$vars$var)]
    )

  # Abort if any new edge violates the tier rule
  .validate_tier_rule(block)
  .validate_forbidden_required(block)

  # Merge into edge table, dropping duplicates, and return updated object
  .kn$edges <- dplyr::distinct(dplyr::bind_rows(.kn$edges, block))
  .validate_forbidden_required(.kn$edges)
  .kn
}

# ---------------------------------------------------------------------------
# public verbs ---------------------------------------------------------------

#' Add variables
#' @export
add_vars <- function(.kn, vars) {
  stopifnot(inherits(.kn, "knowledge"), is.character(vars))

  new_rows <- tibble::tibble(
    var  = setdiff(vars, .kn$vars$var),
    tier = NA_integer_
  )
  .kn$vars <- dplyr::bind_rows(.kn$vars, new_rows)
  .kn
}

#' Assign variables to a tier (numeric or symbolic label)
#' @export
add_tier <- function(.kn, tier, vars) {
  stopifnot(inherits(.kn, "knowledge"), length(tier) == 1)

  res <- .resolve_tier(.kn, tier)
  .kn <- res$kn
  tier_idx <- res$idx

  vars_chr <- .resolved_vars(.kn, {{ vars }})
  .kn <- add_vars(.kn, vars_chr) # adds only truly new names
  idx <- match(vars_chr, .kn$vars$var)
  .kn$vars$tier[idx] <- tier_idx # overwrite NA or previous tier
  .kn
}

#' Register forbidden / required edges
#' @export
forbid_edge <- function(.kn, from, to, edge_type = "directed") {
  .add_edges(.kn, "forbidden", edge_type, {{ from }}, {{ to }})
}

#' @rdname forbid_edge
#' @export
require_edge <- function(.kn, from, to, edge_type = "directed") {
  .add_edges(.kn, "required", edge_type, {{ from }}, {{ to }})
}

# ---------------------------------------------------------------------------
# printing -------------------------------------------------------------------

#' @exportS3Method print knowledge
print.knowledge <- function(x, ...) {
  cli::cat_rule("Knowledge object")
  if (length(x$tier_labels)) {
    cli::cat_line(
      "Tier labels: ",
      paste(names(x$tier_labels), "\u2192", x$tier_labels, collapse = ", ")
    )
  }
  cli::cat_line(cli::style_bold("Variables:"), nrow(x$vars))
  if (nrow(x$vars)) print(x$vars, n = Inf)
  cli::cat_line(cli::style_bold("Edges:"), nrow(x$edges))
  if (nrow(x$edges)) print(x$edges, n = 10)
  invisible(x)
}

# ---------------------------------------------------------------------------
# arithmetic -----------------------------------------------------------------

# internal helper: recompute tier columns ------------------------------------
#' @keywords internal
.update_edge_tiers <- function(.kn) {
  .kn$edges <- dplyr::mutate(
    .kn$edges,
    tier_from = .kn$vars$tier[match(from, .kn$vars$var)],
    tier_to   = .kn$vars$tier[match(to, .kn$vars$var)]
  )
  .kn
}

#' @exportS3Method "+" knowledge
`+.knowledge` <- function(e1, e2) {
  stopifnot(inherits(e1, "knowledge"), inherits(e2, "knowledge"))
  .validate_label_conflict(e1$tier_labels, e2$tier_labels)

  vars <- unique(c(e1$vars$var, e2$vars$var))
  out <- .new_knowledge(vars)

  tiers <- dplyr::bind_rows(e2$vars, e1$vars) |>
    dplyr::group_by(var) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  out$vars$tier <- tiers$tier[match(out$vars$var, tiers$var)]

  out$tier_labels <- c(
    e1$tier_labels,
    e2$tier_labels[setdiff(names(e2$tier_labels), names(e1$tier_labels))]
  )

  out$edges <- dplyr::distinct(dplyr::bind_rows(e1$edges, e2$edges))
  out <- .update_edge_tiers(out)

  .validate_tier_rule(out$edges)
  .validate_forbidden_required(out$edges)
  out
}

# ---------------------------------------------------------------------------
# DSL wrapper ----------------------------------------------------------------

#' Mini-DSL constructor (`tier()`, `forbidden()`, `required()`)
#'
#' @description
#' Accepts an optional data frame followed by calls built from **formulas**:
#'
#' * `tier( 1 ~ V1 + V2, exposure ~ E )`
#' * `forbidden( V1 ~ V4, V2 ~ V4, edge_type = "undirected" )`
#' * `required ( V1 ~ V2 )`
#'
#' @return A populated `knowledge` object.
#' @export
knowledge <- function(...) {
  dots <- as.list(substitute(list(...)))[-1]
  if (!length(dots)) {
    return(.new_knowledge())
  }
  # optional first arg = data frame -----------------------------------------
  df <- NULL
  if (length(dots) && !is.call(dots[[1]])) {
    first <- eval(dots[[1]], parent.frame())
    if (is.data.frame(first)) {
      df <- first
      dots <- dots[-1]
    }
  }
  kn <- if (is.null(df)) .new_knowledge() else .new_knowledge(names(df))

  # helper: tier -------------------------------------------------------------
  tier <- function(...) {
    specs <- rlang::list2(...)
    if (!length(specs)) {
      cli::cli_abort("tier() needs at least one two-sided formula.")
    }

    for (fml in specs) {
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        cli::cli_abort("Each tier() argument must be a two-sided formula.")
      }
      lhs <- rlang::f_lhs(fml)

      vars <- .formula_vars(rlang::f_rhs(fml), kn)
      if (!is.character(vars) || !length(vars)) {
        cli::cli_abort("Tier specification {.code {fml}} matched no variables.")
      }

      kn <<- add_tier(kn, lhs, vars) # lhs may be label *or* number
    }
  }

  # helper: edge helpers -----------------------------------------------------
  edge_helper <- function(status, ..., edge_type = "directed") {
    specs <- rlang::list2(...)
    if (!length(specs)) {
      cli::cli_abort("{.fun {status}}() needs at least one two-sided formula.")
    }

    for (fml in specs) {
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        cli::cli_abort("Arguments must be two-sided formulas.")
      }

      # resolve *expressions* on both sides
      from_vars <- .formula_vars(rlang::f_lhs(fml), kn)
      to_vars <- .formula_vars(rlang::f_rhs(fml), kn)
      if (!is.character(from_vars) || !length(from_vars)) {
        cli::cli_abort("Edge selection {.code {fml}} matched no *from* vars.")
      }
      if (!is.character(to_vars) || !length(to_vars)) {
        cli::cli_abort("Edge selection {.code {fml}} matched no *to* vars.")
      }

      # insert every combination of from × to
      kn <<- .add_edges(kn, status, edge_type, from_vars, to_vars)
    }
  }
  forbidden <- function(..., edge_type = "directed") edge_helper("forbidden", ..., edge_type = edge_type)
  required <- function(..., edge_type = "directed") edge_helper("required", ..., edge_type = edge_type)

  # evaluate the call list ---------------------------------------------------
  allowed <- c("tier", "forbidden", "required")
  for (expr in dots) {
    if (!is.call(expr) || !(as.character(expr[[1]]) %in% allowed)) {
      cli::cli_abort("Only tier(), forbidden(), required() calls are allowed.")
    }
    eval(expr, envir = environment())
  }
  kn
}

# ---------------------------------------------------------------------------
# Java conversion ------------------------------------------------------------

#' Convert to Tetrad `edu.cmu.tetrad.data.Knowledge`
#' @export
as_tetrad_knowledge <- function(.kn) {
  if (!requireNamespace("rJava", quietly = TRUE)) {
    stop("Package 'rJava' is required for as_tetrad().")
  }

  j <- rJava::.jnew("edu/cmu/tetrad/data/Knowledge")

  purrr::pwalk(
    list(.kn$vars$var, .kn$vars$tier),
    function(v, t) if (!is.na(t)) j$addToTier(t, v)
  )
  purrr::pwalk(
    .kn$edges,
    function(status, edge_type, from, to, ...) {
      switch(status,
        forbidden = j$setForbidden(from, to),
        required  = j$setRequired(from, to)
      )
    }
  )
  j
}

# ---------------------------------------------------------------------------
# pcalg conversion -----------------------------------------------------------

#' Convert to a pair of (fixedGaps, fixedEdges) matrices for the **pcalg** package
#'
#' pcalg supports *symmetric* background knowledge only:
#'   • **fixedGaps**  – edges that must be **absent** (undirected)
#'   • **fixedEdges** – edges that must be **present** (undirected)
#'
#' We therefore throw an error if any edges are not undirected.
#'
#' @param .kn A `knowledge` object.
#' @param labels Character vector of variable names in the *exact* order of
#'               your data matrix / data frame.
#' @return A list with elements `fixedGaps` and `fixedEdges`.
#' @export
as_pcalg_constraints <- function(.kn, labels) {
  check_knowledge_obj(.kn)

  if (any(!is.na(.kn$vars$tier))) {
    cli::cli_warn("Tiered background knowledge cannot be utilised by the pcalg engine.")
  }

  p <- length(labels)
  fixedGaps <- matrix(FALSE, p, p, dimnames = list(labels, labels))
  fixedEdges <- matrix(FALSE, p, p, dimnames = list(labels, labels))

  ## helper: map variable → index once --------------------------------------
  idx <- setNames(seq_along(labels), labels)

  ## ---- forbidden ---------------------------------------------------------
  forb <- dplyr::filter(.kn$edges, status == "forbidden")
  if (nrow(forb)) {
    for (k in seq_len(nrow(forb))) {
      i <- idx[[forb$from[k]]]
      j <- idx[[forb$to[k]]]
      if (is.na(i) || is.na(j)) {
        cli::cli_abort("Forbidden edge refers to unknown variable(s).")
      }
      fixedGaps[i, j] <- TRUE
      fixedGaps[j, i] <- TRUE
    }
  }

  ## ---- required ----------------------------------------------------------
  req <- dplyr::filter(.kn$edges, status == "required")
  if (nrow(req)) {
    for (k in seq_len(nrow(req))) {
      i <- idx[[req$from[k]]]
      j <- idx[[req$to[k]]]
      if (is.na(i) || is.na(j)) {
        cli::cli_abort("Required edge refers to unknown variable(s).")
      }
      fixedEdges[i, j] <- TRUE
      fixedEdges[j, i] <- TRUE
    }
  }

  list(fixedGaps = fixedGaps, fixedEdges = fixedEdges)
}


#' Verify that an object is a knowledge
#'
#' @keywords internal
check_knowledge_obj <- function(x) {
  if (!inherits(x, "knowledge")) {
    stop("Input must be a knowledge instance.", .call = FALSE)
  }
  TRUE
}
# ---------------------------------------------------------------------------
# Imports (roxygen tags only) ------------------------------------------------
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows distinct group_by slice ungroup filter mutate
#' @importFrom tidyselect eval_select everything starts_with ends_with
#' @importFrom rlang enquo eval_tidy set_names is_formula f_lhs f_rhs as_name !!
#' @importFrom rlang is_integerish as_string
#' @importFrom purrr pwalk
#' @importFrom cli cat_rule cat_line style_bold cli_abort
NULL
