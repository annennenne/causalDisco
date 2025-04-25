#' @title Create a `knowledge` object
#'
#' @param vars Character vector of variable names.  Defaults to empty.
#' @param frozen Logical. If `TRUE`, no new variables can be added. Defaults to `FALSE`.
#'
#' @return An S3 object of class `"knowledge"`.
#' @keywords internal
.new_knowledge <- function(vars = character(), frozen = FALSE) {
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
      tier_labels = integer(), # named int vector, e.g. c(Monday = 1L)
      frozen = frozen # TRUE means no new vars allowed
    ),
    class = "knowledge"
  )
}

#' @title Supported edge-type strings
#' @keywords internal
.allowed_edge_types <- c("directed", "undirected", "bidirected", "o->", "o-o", "<-o")

#' @title Resolve a tier label or number to an integer index
#'
#' @param .kn A `knowledge` object.
#' @param tier A symbol or string, or an integer.
#'
#' @keywords internal
.resolve_tier <- function(.kn, tier) {
  ## ---------------- integer supplied -------------------------------------
  if (rlang::is_integerish(tier) && length(tier) == 1 && tier >= 1) {
    return(list(idx = as.integer(tier), kn = .kn))
  }

  ## ---------------- symbol / string supplied -----------------------------
  label <- rlang::as_string(tier)
  if (!nzchar(label)) {
    stop("Tier labels must be a non-empty symbol or string.", .call = FALSE)
  }

  if (label %in% names(.kn$tier_labels)) {
    return(list(idx = .kn$tier_labels[[label]], kn = .kn))
  }

  next_idx <- if (length(.kn$tier_labels)) max(.kn$tier_labels) + 1L else 1L
  .kn$tier_labels[[label]] <- next_idx
  list(idx = next_idx, kn = .kn)
}

#' @title Resolve a tidy-select or character spec to character names
#'
#' @param .kn A `knowledge` object.
#' @param spec A tidyselect specification (e.g. `everything()`, `starts_with("V")`) or a character vector.
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

#' @title Extract variable names from the RHS of a `tier()` formula
#'
#' @param rhs A formula (e.g. `1 ~ V1 + V2`).
#' @param .kn A `knowledge` object.
#' @keywords internal
.formula_vars <- function(rhs, .kn) {
  vars <- .resolved_vars(.kn, !!rhs)
  if (length(vars)) {
    return(vars)
  } # tidy-select succeeded
  unique(all.vars(rhs)) # fallback to plain symbols
}

#' @title Validate that no edge runs from higher tier to lower tier
#'
#' @param edges_df A data frame with columns `status`, `edge_type`, `from`, `to`, `tier_from`, and `tier_to`.
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

#' @title Validate that an edge is not simultaneously forbidden *and* required
#'
#' @param edges_df A data frame with columns `status`, `edge_type`, `from`, `to`, `tier_from`, and `tier_to`.
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

#' @title Validate that merging two knowledge objects introduces no tier-label conflict
#'
#' @description A conflict occurs when **two or more different labels map to the same
#' numeric tier index** after the merge (e.g. "January → 1" and "Monday → 1").
#'
#' @param tiers_left A named integer vector of tier labels from the left-hand side.
#' @param tiers_right A named integer vector of tier labels from the right-hand side.
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
    lines <- mapply(
      function(lbls, tier_idx) {
        sprintf(
          "tier %s: %s",
          tier_idx,
          paste(unique(lbls), collapse = ", ")
        )
      },
      conflicts,
      names(conflicts),
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

    stop(
      "Conflicting tier labels detected:\n",
      paste0("  * ", lines, collapse = "\n"),
      call. = FALSE
    )
  }
}




#' @title Add one or many edges to a knowledge object
#'
#' @param .kn A `knowledge` object.
#' @param status A string, either "forbidden" or "required".
#' @param edge_type A string, one of "directed", "undirected", "bidirected", "o->", "o-o", "<-o".
#' @param from A tidyselect specification or character vector of variable names.
#' @param to A tidyselect specification or character vector of variable names.
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

#' @title Add variables to `knowledge` object
#'
#' @description Adds variables to the `knowledge` object. If the object is frozen, an error
#' is thrown if any of the variables are not present in the data frame provided to the object.
#'
#' @param .kn A `knowledge` object.
#' @param vars A character vector of variable names to add.
#'
#' @export
add_vars <- function(.kn, vars) {
  stopifnot(inherits(.kn, "knowledge"), is.character(vars))

  missing <- setdiff(vars, .kn$vars$var)

  if (.kn$frozen && length(missing)) {
    stop(
      "Unknown variable(s): ", paste(missing, collapse = ", "),
      "\nThey are not present in the data frame was provided to this knowledge object.",
      call. = FALSE
    )
  }

  if (length(missing)) {
    new_rows <- tibble::tibble(var = missing, tier = NA_integer_)
    .kn$vars <- dplyr::bind_rows(.kn$vars, new_rows)
  }
  .kn
}


#' @title Add variables to a tier (with positional control)
#'
#' @description
#' Assign one or more variables to a tier (numeric or symbolic).  You can also
#' specify that this tier should come `before` or `after` another tier (by label or index)
#' or relative to existing variables (by name).
#'
#' @param .kn A `knowledge` object.
#' @param tier A symbol (unquoted), string, or integer of length 1 specifying the tier label or index.
#' @param vars A tidyselect specification or character vector of variable names to assign.
#' @param before Optional character or integer vector of tier labels, tier indices, or variable names that this new tier should come before.
#' @param after  Optional character or integer vector of tier labels, tier indices, or variable names that this new tier should come after.
#'
#' @return An updated `knowledge` object with the specified variables assigned to the new tier.
#' @export
add_tier <- function(.kn, tier, vars, before = NULL, after = NULL) {
  # 1) sanity‐check
  if (!inherits(.kn, "knowledge")) {
    stop("`.kn` must be a knowledge object.", call. = FALSE)
  }

  # 2) capture raw exprs
  tier_expr <- rlang::enexpr(tier)
  before_expr <- rlang::enexpr(before)
  after_expr <- rlang::enexpr(after)

  # 3) classify tier into label vs numeric
  if (is.symbol(tier_expr)) {
    tier_label <- as.character(tier_expr)
    numeric_tier <- NULL
  } else if (is.atomic(tier_expr) && length(tier_expr) == 1L) {
    # string or numeric literal
    if (is.character(tier)) {
      tier_label <- tier
      numeric_tier <- NULL
    } else if (is.numeric(tier) && tier >= 1) {
      tier_label <- NULL
      numeric_tier <- as.integer(tier)
    } else {
      stop("`tier` literal must be a single string or integer ≥ 1.", call. = FALSE)
    }
  } else {
    stop("`tier` must be an unquoted name, a single string, or a single integer ≥ 1.",
      call. = FALSE
    )
  }

  # 4) helper: extract a list of raw before/after targets (symbols, strings, or numbers)
  extract_targets <- function(expr) {
    if (is.null(expr)) {
      return(list())
    }
    if (is.symbol(expr)) {
      return(list(as.character(expr)))
    }
    if (is.atomic(expr) && length(expr) == 1L) {
      if (is.numeric(expr)) {
        return(list(as.integer(expr)))
      }
      return(list(as.character(expr)))
    }
    if (is.call(expr) && expr[[1]] == as.name("c")) {
      out <- list()
      for (elt in as.list(expr[-1])) {
        if (is.symbol(elt)) {
          out <- c(out, list(as.character(elt)))
        } else if (is.atomic(elt) && length(elt) == 1L) {
          if (is.numeric(elt)) {
            out <- c(out, list(as.integer(elt)))
          } else {
            out <- c(out, list(as.character(elt)))
          }
        } else {
          stop("`before`/`after` must be symbols or single string/numeric literals.",
            call. = FALSE
          )
        }
      }
      return(out)
    }
    stop("`before`/`after` must be a symbol, literal, or c(...) of them.", call. = FALSE)
  }

  before_targets <- extract_targets(before_expr)
  after_targets <- extract_targets(after_expr)

  # 5) enforce: if tier is a label, you must choose before or after
  if (!is.null(tier_label) && is.null(numeric_tier) &&
    length(before_targets) == 0L && length(after_targets) == 0L) {
    stop(
      "When `tier` is a label (e.g. `Monday`), you must specify either ",
      "`before =` or `after =` to place it relative to existing tiers.",
      call. = FALSE
    )
  }

  # 6) can’t have both before and after
  if (length(before_targets) && length(after_targets)) {
    stop("Specify only one of `before` or `after`.", call. = FALSE)
  }

  # 7) turn each target into an integer index
  resolve_point <- function(x) {
    if (is.numeric(x) && length(x) == 1L) {
      return(as.integer(x))
    }
    if (is.character(x) && x %in% names(.kn$tier_labels)) {
      return(.resolve_tier(.kn, x)$idx)
    }
    if (is.character(x) && x %in% .kn$vars$var) {
      t <- .kn$vars$tier[.kn$vars$var == x]
      if (is.na(t)) {
        stop(sprintf("Variable `%s` has no tier; can’t use in before/after.", x),
          call. = FALSE
        )
      }
      return(t)
    }
    stop(sprintf("`%s` is not a tier label, number, or known variable.", x),
      call. = FALSE
    )
  }

  # 8) compute insertion index
  if (length(before_targets)) {
    pts <- vapply(before_targets, resolve_point, integer(1))
    insert_idx <- min(pts)
  } else if (length(after_targets)) {
    pts <- vapply(after_targets, resolve_point, integer(1))
    insert_idx <- max(pts) + 1L
  } else {
    # only numeric tiers fall through here
    insert_idx <- if (length(.kn$tier_labels)) {
      max(.kn$tier_labels) + 1L
    } else {
      1L
    }
  }

  # 9) if brand-new label, bump existing tiers & labels ≥ insert_idx
  if (!is.null(tier_label) && !(tier_label %in% names(.kn$tier_labels))) {
    # 9a) bump variable tiers
    to_bump_vars <- which(!is.na(.kn$vars$tier) & .kn$vars$tier >= insert_idx)
    if (length(to_bump_vars)) {
      .kn$vars$tier[to_bump_vars] <- .kn$vars$tier[to_bump_vars] + 1L
    }
    # 9b) bump label→index map
    bump_lbls <- .kn$tier_labels >= insert_idx
    if (any(bump_lbls)) {
      .kn$tier_labels[bump_lbls] <- .kn$tier_labels[bump_lbls] + 1L
    }
    # 9c) insert & sort
    .kn$tier_labels[[tier_label]] <- insert_idx
    .kn$tier_labels <- .kn$tier_labels[order(.kn$tier_labels)]
    tier_idx <- insert_idx

    # 10) else if a numeric literal
  } else if (!is.null(numeric_tier)) {
    tier_idx <- numeric_tier

    # 11) else must be an existing label
  } else {
    res <- .resolve_tier(.kn, tier_label)
    .kn <- res$kn
    tier_idx <- res$idx
  }

  # 12) finally: capture & add your vars
  vars_expr <- substitute(vars)
  if (is.call(vars_expr)) {
    syms <- all.vars(vars_expr)
    vars_chr <- setdiff(syms, as.character(vars_expr[[1]]))
  } else if (is.symbol(vars_expr)) {
    vars_chr <- as.character(vars_expr)
  } else if (is.character(vars) && length(vars) > 0L) {
    vars_chr <- vars
  } else {
    vars_chr <- character()
  }

  .kn <- add_vars(.kn, vars_chr)
  idxs <- match(vars_chr, .kn$vars$var)
  .kn$vars$tier[idxs] <- tier_idx

  # 13) sort the variables by tier, then alphabetically
  .kn$vars <- dplyr::arrange(.kn$vars, tier)
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

  df <- NULL
  if (length(dots) && !is.call(dots[[1]])) {
    first <- eval(dots[[1]], parent.frame())
    if (is.data.frame(first)) {
      df <- first
      dots <- dots[-1]
    }
  }
  kn <- if (is.null(df)) .new_knowledge() else .new_knowledge(names(df), frozen = TRUE)

  # helper: tier -------------------------------------------------------------
  # Replacement for the internal `tier()` helper in your `knowledge()` constructor:
  tier <- function(...) {
    specs <- rlang::list2(...)
    if (!length(specs)) {
      stop("tier() needs at least one two-sided formula.", call. = FALSE)
    }

    for (fml in specs) {
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        stop("Each tier() argument must be a two-sided formula.", call. = FALSE)
      }

      # LHS expression (could be a bare number or a name) and RHS vars
      lhs_expr <- rlang::f_lhs(fml)
      rhs_expr <- rlang::f_rhs(fml)
      vars <- .formula_vars(rhs_expr, kn)
      if (!is.character(vars) || !length(vars)) {
        cli::cli_abort("Tier specification {.code {fml}} matched no variables.")
      }

      # Try to eval the LHS; if it's a single number, we'll treat it as a numeric tier
      lhs_val <- tryCatch(
        rlang::eval_tidy(lhs_expr, env = environment()),
        error = function(e) NULL
      )

      if (is.numeric(lhs_val) && length(lhs_val) == 1 && !is.na(lhs_val)) {
        # ── Numeric tier: pass an atomic integer literal
        args <- list(
          .kn  = kn,
          tier = as.integer(lhs_val),
          vars = vars
        )
      } else {
        # ── Labeled tier: auto‐append after the current last tier
        tier_label <- rlang::as_string(lhs_expr)
        last_idx <- if (length(kn$tier_labels)) max(kn$tier_labels) else 0L

        args <- list(
          .kn   = kn,
          tier  = tier_label,
          vars  = vars,
          after = last_idx
        )
      }

      kn <<- do.call(add_tier, args)
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
        stop("Arguments must be two-sided formulas.", .call = FALSE)
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
      stop("Only tier(), forbidden(), required() calls are allowed.", .call = FALSE)
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
        stop("Forbidden edge refers to unknown variable(s).", .call = FALSE)
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
        stop("Required edge refers to unknown variable(s).", .call = FALSE)
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
