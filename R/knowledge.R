# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Public API  ──────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Knowledge Mini-DSL constructor (`tier()`, `forbidden()`, `required()`)
#'
#' @description
#' Accepts an optional data frame followed by calls built from **formulas**:
#'
#' * `tier( 1 ~ V1 + V2, exposure ~ E )`
#' * `forbidden( V1 ~ V4, V2 ~ V4, edge_type = "undirected" )`
#' * `required ( V1 ~ V2 )`
#'
#' @details
#' The first argument can be a data frame, which will be used to populate the
#' `knowledge` object with variable names. If you later add variables with
#' add_* verbs, this will throw a warning, since the knowledge object will
#' be *frozen*. You can unfreeze a knowledge object by using the function
#' `unfreeze(knowledge)`.
#'
#' If no data frame is provided, the
#' `knowledge` object will be empty until variables are added with `tier()`,
#' `forbidden()`, or `required()`. You can also populate the object with
#' the `add_vars()` verb.
#'
#' `tier()` assigns variables to tiers. Tiers are internally numbered, starting
#' with 1. If you provide a numeric literal, it will be used as the tier index.
#' If you provide a symbol or string, it will be used as a label. The order of
#' the provided tiers will be used as the order of the tiers in the object,
#' unless tiers are specified with numeric literals. This function takes
#' formulas as input. The left-hand side of the formula is the *tier* and the
#' right-hand side is the *variables*. You can also use tidyselect syntax to
#' specify the variables. For example, `tier(1 ~ starts_with("V"))` will assign
#' all variables starting with "V" to tier 1.
#'
#' The `forbidden()` and `required()` functions add edges to the knowledge
#' object. The edges are added as directed edges by default, but you can specify
#' the `edge_type` argument to use undirected or bidirected edges. There is
#' currently also support for PAG-type edges, but these are generally not
#' supported in other packages. These functions also take formulas as input.
#' The left-hand side of the formula is the *from* variable, and the right-hand
#' side is the *to* variable.
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
  kn <- if (is.null(df)) {
    .new_knowledge()
  } else {
    .new_knowledge(names(df), frozen = TRUE)
  }

  tier <- function(...) {
    specs <- rlang::list2(...)
    if (!length(specs)) {
      stop("tier() needs at least one two-sided formula.", .call = FALSE)
    }

    for (fml in specs) {
      if (inherits(fml, "tier_bundle")) {
        for (g in fml) tier(g) # recurse on each generated formula
        next
      }
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        stop("Each tier() argument must be a two-sided formula.", .call = FALSE)
      }

      lhs_expr <- rlang::f_lhs(fml)
      rhs_expr <- rlang::f_rhs(fml)

      ## --------------------------------------------------------------- ##
      ## 1. Collect the RHS variables once ----------------------------- ##
      ## --------------------------------------------------------------- ##
      vars <- .formula_vars(kn, rhs_expr)
      if (!length(vars)) {
        stop(
          sprintf(
            "Tier specification %s matched no variables.",
            paste(deparse(fml), collapse = "")
          ),
          call. = FALSE
        )
      }

      ## --------------------------------------------------------------- ##
      ## 2. Decide what the LHS means ---------------------------------- ##
      ## --------------------------------------------------------------- ##
      lhs_val <- tryCatch(
        rlang::eval_tidy(lhs_expr, env = parent.frame()),
        error = function(...) NULL
      )

      if (is.numeric(lhs_val) && length(lhs_val) == 1 && !is.na(lhs_val)) {
        # numeric tier  →  keep index exactly as given
        new_fml <- rlang::new_formula(lhs_val, rhs_expr, env = rlang::empty_env())
        kn <<- add_to_tier(kn, new_fml) # plain call, no positioning
      } else {
        # labelled tier → append (or position) -------------------------
        tier_label <- rlang::as_string(lhs_expr)

        current_idx <- c(kn$tier_labels, kn$vars$tier)
        current_idx <- current_idx[!is.na(current_idx)]
        last_idx <- if (length(current_idx)) max(current_idx) else 0L

        new_fml <- rlang::new_formula(lhs_expr, rhs_expr, env = rlang::empty_env())
        kn <<- rlang::inject(
          add_to_tier(kn, !!new_fml, after = !!last_idx)
        )
      }
    }
  }



  # helper: edge helpers -----------------------------------------------------
  edge_helper <- function(status, ..., edge_type = "directed") {
    specs <- rlang::list2(...)
    if (!length(specs)) {
      stop(
        sprintf("%s() needs at least one two-sided formula.", status),
        call. = FALSE
      )
    }

    for (fml in specs) {
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        stop("Arguments must be two-sided formulas.", .call = FALSE)
      }

      # resolve *expressions* on both sides
      from_vars <- .formula_vars(kn, rlang::f_lhs(fml))
      to_vars <- .formula_vars(kn, rlang::f_rhs(fml))
      if (!is.character(from_vars) || !length(from_vars)) {
        stop(
          sprintf(
            "Edge selection `%s` matched no *from* vars.",
            rlang::expr_deparse(fml)
          ),
          call. = FALSE
        )
      }

      if (!is.character(to_vars) || !length(to_vars)) {
        stop(
          sprintf(
            "Edge selection `%s` matched no *to* vars.",
            rlang::expr_deparse(fml)
          ),
          call. = FALSE
        )
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

# ────────────────────────────────── Verbs ─────────────────────────────────────

#' @title Add variables to `knowledge` object
#'
#' @description Adds variables to the `knowledge` object. If the object is
#' frozen, an error is thrown if any of the variables are not present in the
#' data frame provided to the object.
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
#' @param ... One or more two-sided formulas, where the left-hand side is the
#' tier and the right-hand side is the variable(s) to assign to that tier.
#' @param before Optional character or integer vector of tier labels, tier indices, or variable names that this new tier should come before.
#' @param after  Optional character or integer vector of tier labels, tier indices, or variable names that this new tier should come after.
#'
#' @return An updated `knowledge` object with the specified variables assigned to the new tier.
#' @export
add_to_tier <- function(.kn, ..., before = NULL, after = NULL) {
  check_knowledge_obj(.kn)

  specs <- rlang::list2(...)
  if (!length(specs)) {
    stop(
      "add_tier() needs at least one two-sided formula.",
      call. = FALSE
    )
  }

  ## ---- 1. validate before/after combination --------------------------------
  if (!missing(before) && !missing(after)) {
    stop(
      "Specify only one of `before` or `after`.",
      call. = FALSE
    )
  }
  if (length(specs) > 1L && (!missing(before) || !missing(after))) {
    stop(
      "`before` / `after` can position only one tier at a time.",
      call. = FALSE
    )
  }

  ## ---- 2. resolve anchor tiers once  -------------------------------------
  anchor_idx <- if (!missing(before)) {
    .tiers_from_spec(.kn, rlang::enexpr(before))
  } else if (!missing(after)) {
    .tiers_from_spec(.kn, rlang::enexpr(after))
  } else {
    integer(0)
  }

  insert_idx <- if (!length(anchor_idx)) {
    NA_integer_
  } else if (!missing(before)) {
    min(anchor_idx)
  } else {
    max(anchor_idx) + 1L
  }

  # -------------------------------------------------------------------------
  # walk over each formula ---------------------------------------------------
  for (k in seq_along(specs)) {
    fml <- specs[[k]]
    if (!rlang::is_formula(fml, lhs = TRUE)) {
      stop(
        "Each argument must be a two-sided formula.",
        call. = FALSE
      )
    }
    lhs <- rlang::f_lhs(fml)
    rhs <- rlang::f_rhs(fml)
    vars <- .formula_vars(.kn, rhs)
    if (!length(vars)) {
      stop(
        sprintf(
          "Tier specification `%s` matched no variables.",
          rlang::expr_deparse(fml)
        ),
        call. = FALSE
      )
    }
    # --- numeric LHS --------------------------------------------------------
    lhs_val <- tryCatch(rlang::eval_tidy(lhs, env = parent.frame()),
      error = function(...) NULL
    )
    if (is.numeric(lhs_val) && length(lhs_val) == 1 && !is.na(lhs_val)) {
      tier_idx <- as.integer(lhs_val)
      tier_label <- NULL
      if (k == 1L && !is.na(insert_idx)) {
        cli::cli_warn("Ignoring `before/after`: numeric tier is absolute.")
      }
    } else {
      # --- symbol / string LHS ---------------------------------------------
      tier_label <- rlang::as_string(lhs)
      parsed <- .parse_tier(.kn, lhs) # resolves existing label
      tier_idx <- parsed$idx
      .kn <- parsed$kn
      is_new_label <- is.na(tier_idx)

      # decide final index ---------------------------------------------------
      if (!is.na(insert_idx) && k == 1L) {
        # user gave before/after → use it (even for existing label, we move it)
        if (is_new_label) {
          .kn <- .bump_tiers_up_from(.kn, insert_idx)
          .kn$tier_labels[[tier_label]] <- insert_idx
        } else {
          .kn <- .bump_tiers_up_from(.kn, insert_idx)
        }
        tier_idx <- insert_idx
      } else if (is_new_label) {
        current <- c(.kn$tier_labels, .kn$vars$tier)
        current <- current[!is.na(current)]
        tier_idx <- if (length(current)) max(current) + 1L else 1L
        .kn <- .bump_tiers_up_from(.kn, tier_idx)
        .kn$tier_labels[[tier_label]] <- tier_idx
      }
    }

    # register variables -----------------------------------------------------
    .kn <- add_vars(.kn, vars)
    .kn$vars$tier[match(vars, .kn$vars$var)] <- tier_idx
  }

  .kn$vars <- dplyr::arrange(.kn$vars, tier, var)
  .kn
}



#' @title Add a forbidden edge to a knowledge object
#'
#' @description These edges are not allowed to be present in the final graph.
#' You can specify these edges as directed, undirected, or bidirected. The default is
#' directed. You can also specify the edge type as "o->" or "o-o" for PAG-type edges.
#'
#' @param .kn A `knowledge` object.
#' @param ... Either a two-sided formula (`A ~ C`) *or* `from`, `to`.
#' @param edge_type A string, one of "directed", "undirected", "bidirected", "o->", "o-o"
#' @export
forbid_edge <- function(.kn, ..., edge_type = "directed") {
  dots <- rlang::enquos(...)
  if (length(dots) == 1) {
    .edge_verb(.kn, "forbidden", dots[[1]], NULL, edge_type)
  } else if (length(dots) == 2) {
    .edge_verb(.kn, "forbidden", dots[[1]], dots[[2]], edge_type)
  } else {
    stop("forbid_edge() takes either 1 or 2 edge specifications.", .call = FALSE)
  }
}

#' @title Add a forbidden edge to a knowledge object
#'
#' @description These edges are not allowed to be present in the final graph.
#' You can specify these edges as directed, undirected, or bidirected. The default is
#' directed. You can also specify the edge type as "o->" or "o-o" for PAG-type edges.
#'
#' @inheritParams forbid_edge
#' @export
require_edge <- function(.kn, ..., edge_type = "directed") {
  dots <- rlang::enquos(...)
  if (length(dots) == 1) {
    .edge_verb(.kn, "required", dots[[1]], NULL, edge_type)
  } else if (length(dots) == 2) {
    .edge_verb(.kn, "required", dots[[1]], dots[[2]], edge_type)
  } else {
    stop("require_edge() takes either 1 or 2 edge specifications.", .call = FALSE)
  }
}

#' @title Unfreeze a `knowledge` object.
#'
#' @description This allows you to add new variables to the `knowledge` object,
#' even though it was frozen earlier by adding a data frame to the knowledge
#' constructor `knowledge()`.
#'
#' @param .kn A `knowledge` object.
#' @return The same `knowledge` object with the `frozen` attribute set to `FALSE`.
unfreeze <- function(.kn) {
  check_knowledge_obj(.kn)
  .kn$frozen <- FALSE
  .kn
}

# ────────────────────────────────── Print ─────────────────────────────────────

#' @title Print a `knowledge` object
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

# ────────────────────────────────── Merge ─────────────────────────────────────

#' @title Merge two `knowledge` objects
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

# ────────────────────────────────── Check ─────────────────────────────────────

#' @title Verify that an object is a knowledge
#'
#' @description Check that the object is a `knowledge` object. Mostly
#' for internal use in causalDisco.
#'
#' @param x Object to check.
#' @keywords internal
check_knowledge_obj <- function(x) {
  if (!inherits(x, "knowledge")) {
    stop("Input must be a knowledge instance.", .call = FALSE)
  }
  TRUE
}

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────── Conversion to Tetrad, pcalg, bnlearn  ─────────────────────
# ──────────────────────────────────────────────────────────────────────────────


#' @title Convert to Tetrad `edu.cmu.tetrad.data.Knowledge`
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
#'   • **fixedGaps**  – **forbidden** edges (undirected)
#'   • **fixedEdges** – **required** edges (undirected)
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
    warning(
      "Tiered background knowledge cannot be utilised by the pcalg engine.",
      "\n  This cannot be resolved by setting forbidden edges as pcalg",
      " does not support directed fobidden edges."
    )
  }

  # Initialize
  p <- length(labels)
  fixedGaps <- matrix(FALSE, p, p, dimnames = list(labels, labels))
  fixedEdges <- matrix(FALSE, p, p, dimnames = list(labels, labels))

  # Create a named index for the labels
  idx <- setNames(seq_along(labels), labels)

  # Find out if there are any non-undirected edges.
  bad_edges <- .kn$edges |>
    filter(edge_type != "undirected") |>
    mutate(
      # Create descriptive string of edges
      desc = paste0("From ", from, " to ", to, " with edge type: ", edge_type)
    ) |>
    pull(desc)

  if (length(bad_edges)) {
    stop(
      "pcalg does not support asymmetric edges.\n",
      "The following edges are not undirected:\n",
      paste0("  * ", bad_edges, collapse = "\n"),
      call. = FALSE
    )
  }

  # Forbidden edges
  forb <- dplyr::filter(.kn$edges, status == "forbidden")
  if (nrow(forb)) {
    for (k in seq_len(nrow(forb))) {
      i <- idx[[forb$from[k]]]
      j <- idx[[forb$to[k]]]
      if (is.na(i) || is.na(j)) {
        stop("Forbidden edge refers to unknown variable(s).", .call = FALSE)
      }
      # pcalg does not support directed edges
      fixedGaps[i, j] <- TRUE
      fixedGaps[j, i] <- TRUE
    }
  }

  # Required edges
  req <- dplyr::filter(.kn$edges, status == "required")
  if (nrow(req)) {
    for (k in seq_len(nrow(req))) {
      i <- idx[[req$from[k]]]
      j <- idx[[req$to[k]]]
      if (is.na(i) || is.na(j)) {
        stop("Required edge refers to unknown variable(s).", .call = FALSE)
      }
      # pcalg does not support directed edges
      fixedEdges[i, j] <- TRUE
      fixedEdges[j, i] <- TRUE
    }
  }

  list(fixedGaps = fixedGaps, fixedEdges = fixedEdges)
}

# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Internal helpers  ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ────────────────────────────── New knowledge  ────────────────────────────────
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

# ────────────────────────────── Allowed edges  ────────────────────────────────

#' @title Supported edge-type strings
#' @keywords internal
.allowed_edge_types <- c("directed", "undirected", "bidirected", "o->", "o-o")

# ─────────────────────────── Validation helpers  ──────────────────────────────
#' @title Validate that no edge runs from higher tier to lower tier
#'
#' @param edges_df A data frame with columns `status`, `edge_type`, `from`,
#' `to`, `tier_from`, and `tier_to`.
#' @keywords internal
.validate_tier_rule <- function(edges_df) {
  tier_violations <- dplyr::filter(
    edges_df,
    !is.na(tier_from),
    !is.na(tier_to),
    edge_type %in% c("directed", "bidirected"),
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
#' @param edges_df A data frame with columns `status`, `edge_type`, `from`,
#' `to`, `tier_from`, and `tier_to`.
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

  # Check for conflicts: split by numeric tier and filter for >1 unique label
  conflicts <- Filter(
    function(label) length(unique(label)) > 1,
    split(names(merged), merged) # labels grouped by numeric tier
  )
  # Print conflict message if any exist
  if (length(conflicts)) {
    lines <- mapply(
      function(labels, tier_idx) {
        sprintf(
          "tier %s: %s",
          tier_idx,
          paste(unique(labels), collapse = ", ")
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

# ───────────────────────────── Tier helpers  ──────────────────────────────────

#' Resolve a tier specification to an index or (new) label
#'
#' @description
#' Turns the user-supplied `tier` argument of **`add_to_tier()`** into a
#'   deterministic result:
#'
#' * __Numeric literal__ (`1`, `3L`): returns that number.
#' * __Existing label__  (`Monday`): returns its mapped index.
#' * __Brand-new label__ (any other symbol/character) →  marks it for creation
#'   by returning `NA` together with the textual label.
#'
#' @param .kn A `knowledge` object.
#' @param tier A symbol, character string, or single positive integer.
#'
#' @return A named list with components
#'   * `idx`   `integer(1)` — resolved tier index (or `NA` if the label is new)
#'   * `label` `character(1)`|`NULL` — the label if one was supplied
#'   * `kn`    the (unchanged) `knowledge` object
#'
#' @keywords internal
.parse_tier <- function(.kn, tier) {
  if (rlang::is_integerish(tier) && length(tier) == 1 && tier >= 1) {
    return(list(idx = as.integer(tier), label = NULL, kn = .kn))
  }

  label <- rlang::as_string(tier)
  if (!nzchar(label)) {
    stop("`tier` must be a number ≥ 1 or a non-empty label.", .call = FALSE)
  }

  if (label %in% names(.kn$tier_labels)) {
    return(list(idx = .kn$tier_labels[[label]], label = label, kn = .kn))
  }

  list(idx = NA_integer_, label = label, kn = .kn)
}


#' @title Translate a *before/after* position specifcation into tier indices
#'
#' @description
#' Helper for **`add_to_tier()`** that converts the unevaluated expression given
#'   to `before =` / `after =` into a vector of tier indices:
#'
#' * Single symbol or string: tier label or variable name
#' * Numeric literal: numeric tier index
#' * `c(...)` mix (any of the above): flattened, combined result
#'
#' Errors if a referenced variable has no tier or if an element cannot be
#'   matched to either a tier label, tier index, or variable.
#'
#' @param .kn A `knowledge` object.
#' @param x Unevaluated expression captured from the user (e.g. `quote(c(A,2))`).
#'
#' @return An integer vector of tier indices (possibly empty).
#' @keywords internal
.tiers_from_spec <- function(.kn, x) {
  if (rlang::is_null(x)) {
    return(integer())
  }

  # Extract the unevaluated expression(s)
  parts <- rlang::exprs(!!x)

  # If the user wrote `c(A, B, 2)` split the call apart
  if (length(parts) == 1 && rlang::is_call(parts[[1]], "c")) {
    parts <- rlang::call_args(parts[[1]])
  }

  # Walk over every element and turn it into a tier index
  unlist(lapply(
    parts,
    # Function that resolves element to a tier index
    function(elm) {
      # Numeric literal
      if (rlang::is_integerish(elm)) {
        return(as.integer(elm))
      }
      # Symbol or string
      if (rlang::is_symbol(elm) || is.character(elm)) {
        label <- rlang::as_string(elm)

        # Matches a tier label
        if (label %in% names(.kn$tier_labels)) {
          return(.kn$tier_labels[[label]])
        }

        # Matches a variable name
        if (label %in% .kn$vars$var) {
          # Look up its tier
          t <- .kn$vars$tier[.kn$vars$var == label]

          # Variable exists but has not tier assigned
          if (is.na(t)) {
            stop(
              sprintf(
                "Variable `%s` has no tier; cannot use in `before/after`.",
                label
              ),
              call. = FALSE
            )
          }
          return(t)
        }
      }

      # Invalid specification, throw error
      stop(
        sprintf(
          "`%s` is not a tier label, index, or variable.",
          rlang::as_label(el)
        ),
        call. = FALSE
      )
    }
  ))
}


#' @title Shift all tiers **≥** a position *up* by one
#'
#' @description
#' When inserting a brand-new tier in the middle of the existing ordering
#' we need to “open a slot”.
#' This helper increments
#' * the `tier` column of every affected variable, and
#' * each value in `.kn$tier_labels`
#'
#' that is **greater than or equal to** `insert_idx`.
#'
#' @param .kn A `knowledge` object.
#' @param insert_idx  Integer index at which the new tier will be inserted.
#'
#' @return The modified `knowledge` object with bumped indices.
#' @keywords internal
.bump_tiers_up_from <- function(.kn, insert_idx) {
  .kn$vars$tier <- ifelse(!is.na(.kn$vars$tier) & .kn$vars$tier >= insert_idx,
    .kn$vars$tier + 1L,
    .kn$vars$tier
  )
  to_bump <- .kn$tier_labels >= insert_idx
  .kn$tier_labels[to_bump] <- .kn$tier_labels[to_bump] + 1L
  .kn
}

#' @title Recompute the `tier_from` and `tier_to` columns of the edges dataframe
#'
#' @description Used internally in `+.knowledge`.
#' @keywords internal
.update_edge_tiers <- function(.kn) {
  .kn$edges <- dplyr::mutate(
    .kn$edges,
    tier_from = .kn$vars$tier[match(from, .kn$vars$var)],
    tier_to   = .kn$vars$tier[match(to, .kn$vars$var)]
  )
  .kn
}

# ───────────────────────────── Edge helpers  ──────────────────────────────────
#' @title Add one or many edges to a knowledge object
#'
#' @param .kn A `knowledge` object.
#' @param status A string, either "forbidden" or "required".
#' @param edge_type A string, one of "directed", "undirected", "bidirected", "o->", "o-o"
#' @param from A tidyselect specification or character vector of variable names.
#' @param to A tidyselect specification or character vector of variable names.
#' @keywords internal
.add_edges <- function(.kn, status, edge_type, from, to) {
  # Reject unknown edge types
  if (!(edge_type %in% .allowed_edge_types)) {
    stop(
      sprintf(
        "edge_type must be one of %s.",
        paste(.allowed_edge_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Resolve `from` / `to` specs into character vectors of variable names
  from_chr <- .vars_from_spec(.kn, {{ from }})
  to_chr <- .vars_from_spec(.kn, {{ to }})

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

  # Abort if any new edge violates the forbidden/required rule
  .validate_forbidden_required(block)

  # Merge into edge table, dropping duplicates, and return updated object
  .kn$edges <- dplyr::distinct(dplyr::bind_rows(.kn$edges, block))

  # Validate again for safety
  .validate_forbidden_required(.kn$edges)
  .kn
}

#' @title Handle `forbid_edge()` and `require_edge()` verbs
#'
#' @description Help `forbid_edge()` and `require_edge()` to handle both
#' tidyselect and formula inputs as well as standard `to`, `from` arguments.
#' Passes arguments correctly to `.add_edges()`.
#'
#' @param .kn A `knowledge` object.
#' @param status A string, either "forbidden" or "required".
#' @param from A tidyselect specification or a variable name string or symbol.
#' @param to A tidyselect specification or a variable name string or symbol.
#' @param edge_type A string, one of "directed", "undirected", "bidirected", "o->", "o-o"
#' @keywords internal
.edge_verb <- function(.kn, status,
                       from_quo, to_quo = NULL,
                       edge_type = "directed") {
  # Check if `from_quo` is a formula
  if (rlang::quo_is_call(from_quo, "~") && is.null(to_quo)) {
    fml <- rlang::get_expr(from_quo)
    from_vars <- .formula_vars(.kn, rlang::f_lhs(fml))
    to_vars <- .formula_vars(.kn, rlang::f_rhs(fml))
  } else {
    # from_quo is not a formula
    # Use .vars_from_spec to resolve the specification
    from_vars <- .vars_from_spec(.kn, !!from_quo)
    to_vars <- .vars_from_spec(.kn, !!to_quo)

    if (!length(from_vars) || !length(to_vars)) {
      stop(
        paste0(
          "forbid_edge()/require_edge() need either a two-sided ",
          "formula `A ~ B` or both `from` and `to`."
        ),
        call. = FALSE
      )
    }
  }
  .add_edges(.kn, status, edge_type, from_vars, to_vars)
}



# ───────────────────────────── Misc helpers  ──────────────────────────────────
#' @title Resolve a tidy-select or character spec to character names
#'
#' @param .kn A `knowledge` object.
#' @param spec A tidyselect specification (e.g. `everything()`,
#' `starts_with("V")`) or a character vector.
#' @keywords internal
.vars_from_spec <- function(.kn, spec) {
  lookup <- rlang::set_names(seq_along(.kn$vars$var), .kn$vars$var)

  tryCatch(
    # If possible, use tidyselect to resolve the specifation to variable names
    # (e.g. `starts_with("V")` → `c("V1", "V2", ...)`)
    # This will throw an error if the spec is not valid
    names(tidyselect::eval_select(rlang::enquo(spec), lookup)),
    error = function(e) {
      out <- tryCatch(
        # If not, try to evaluate the expression directly
        rlang::eval_tidy(rlang::enquo(spec)),
        error = function(...) NULL
      )
      # If the result is a character vector, return it
      if (is.character(out)) {
        return(out)
      }
      # Otherwise, return an empty character vector
      character(0)
    }
  )
}

#' @title Extract variable names from the RHS of a `tier()` formula
#'
#' @param .kn A `knowledge` object.
#' @param rhs A formula (e.g. `1 ~ V1 + V2`).
#' @keywords internal
.formula_vars <- function(.kn, rhs) {
  vars <- .vars_from_spec(.kn, !!rhs)
  if (length(vars)) {
    return(vars)
  } # tidy-select succeeded
  unique(all.vars(rhs)) # fallback to plain symbols
}

#' Generate multiple tier–variable formulas in one call
#'
#' @param tiers     Positive integers – the tier indices to create.
#' @param prefix    Constant string that *precedes* the tier number
#'                  (e.g. `"tier"` for names like `"tier3_X"`).
#' @param suffix    Constant string that *follows* the tier number
#'                  (e.g. `"_"`     for names like `"X_3"`).
#' @param pattern  General pattern containing the token `{i}` that will be
#'                  replaced by the current tier number.  The resulting string
#'                  must match the variable **exactly**.
#'
#' Exactly **one** of `prefix`, `suffix`, or `pattern` must be supplied.
#'
#' @return An object of class `"tier_bundle"` that `tier()` expands
#'         automatically.
#' @examples
#' tier(seq_tiers(1:3, suffix = "_{i}")) # X_1, X_2, X_3
#' tier(seq_tiers(1:2, prefix = "tier")) # tier1_Y, tier2_Y
#' tier(seq_tiers(c(4, 6), pattern = "Z{i}_exact")) # Z4_exact, Z6_exact
seq_tiers <- function(tiers,
                      prefix = NULL,
                      suffix = NULL,
                      pattern = NULL) {
  stopifnot(
    is.numeric(tiers), all(tiers >= 1),
    xor(!is.null(prefix), xor(!is.null(suffix), !is.null(pattern)))
  )

  if (!is.null(pattern) && !grepl("{i}", pattern, fixed = TRUE)) {
    stop("`pattern` must contain the token \"{i}\".", call. = FALSE)
  }

  build_formula <- function(i) {
    if (!is.null(prefix)) {
      pat <- if (grepl("{i}", prefix, fixed = TRUE)) {
        gsub("{i}", i, prefix, fixed = TRUE) # token already inside
      } else {
        paste0(prefix, i) # append number
      }
      rhs <- rlang::expr(starts_with(!!pat))
    } else if (!is.null(suffix)) {
      pat <- if (grepl("{i}", suffix, fixed = TRUE)) {
        gsub("{i}", i, suffix, fixed = TRUE)
      } else {
        paste0(suffix, i) # append number
      }
      rhs <- rlang::expr(ends_with(!!pat))
    } else {
      pat <- gsub("{i}", i, pattern, fixed = TRUE)
      rhs <- rlang::expr(all_of(!!pat))
    }
    rlang::new_formula(i, rhs, env = rlang::empty_env())
  }

  structure(lapply(tiers, build_formula), class = "tier_bundle")
}



# ──────────────────────────────── Imports ─────────────────────────────────────

#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows distinct group_by slice ungroup filter mutate
#' @importFrom tidyselect eval_select everything starts_with ends_with
#' @importFrom rlang enquo eval_tidy set_names is_formula f_lhs f_rhs as_name !!
#' @importFrom rlang is_integerish as_string
#' @importFrom purrr pwalk
#' @importFrom cli cat_rule cat_line style_bold
NULL
