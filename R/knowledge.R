# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Public API  ──────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Knowledge Mini-DSL constructor (`tier()`, `forbidden()`, `required()`)
#'
#' @description
#' Accepts an optional data frame followed by calls built from **formulas**:
#'
#' * `tier( 1 ~ V1 + V2, exposure ~ E )`
#' * `forbidden( V1 ~ V4, V2 ~ V4)`
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
#' object. The edges are added as directed edges by default, currently there is
#' no theoretical support for other edge types than these.
#' These functions also take formulas as input.
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

    # numeric-vector shortcut
    if (length(specs) == 1L &&
      is.numeric(specs[[1]]) &&
      is.atomic(specs[[1]])) {
      vec <- specs[[1]]
      vars <- kn$vars$var

      if (!length(vars)) {
        stop("Using tier(<numeric vector>) needs the data-frame columns first.",
          call. = FALSE
        )
      }
      if (length(vec) != length(vars)) {
        stop("Tier vector length (", length(vec),
          ") must equal number of variables (", length(vars), ").",
          call. = FALSE
        )
      }

      # check for duplicates in the tier catalog
      dup <- intersect(vec, kn$tiers$idx)
      if (length(dup)) {
        stop("Tier index(es) ", paste(dup, collapse = ", "),
          " already exist.",
          call. = FALSE
        )
      }

      # add numeric-only rows
      new_tiers <- tibble::tibble(
        idx   = vec,
        label = NA_character_
      )
      kn$tiers <<- dplyr::arrange(
        dplyr::bind_rows(kn$tiers, new_tiers),
        idx, label
      )

      # assign tiers to variables
      kn$vars <<- dplyr::mutate(
        kn$vars,
        tier = vec
      )

      return(kn)
    }

    if (!length(specs)) {
      stop("tier() needs at least one two-sided formula.", call. = FALSE)
    }

    # loop over each formula
    for (fml in specs) {
      # tier bundle created by seq_tier
      if (inherits(fml, "tier_bundle")) {
        tier_vec <- integer(length(kn$vars$var))

        # fill the tier vector with the bundle index
        for (g in fml) {
          idx <- as.integer(rlang::f_lhs(g)) # 1, 2, …
          rhs <- rlang::f_rhs(g) # ends_with("_{i}") etc.

          pos <- tidyselect::eval_select(rhs, setNames(seq_len(length(kn$vars$var)), kn$vars$var))
          if (length(pos) == 0) {
            stop("Pattern ", deparse(rhs), " matched no variables.",
              call. = FALSE
            )
          }

          if (any(tier_vec[pos] != 0L)) {
            stop("Some variables matched by two patterns: ",
              paste(kn$vars$var[pos[tier_vec[pos] != 0L]], collapse = ", "),
              call. = FALSE
            )
          }

          tier_vec[pos] <- idx
        }

        # insert new indexes
        missing_idx <- setdiff(unique(tier_vec[tier_vec > 0L]), kn$tiers$idx)
        if (length(missing_idx)) {
          kn$tiers <<- dplyr::arrange(
            dplyr::bind_rows(
              kn$tiers,
              tibble::tibble(idx = sort(missing_idx), label = NA_character_)
            ),
            idx, label
          )
        }

        # do not assign to zero indexes
        mask <- tier_vec > 0L

        # assign
        kn$vars$tier[mask] <<- tier_vec[mask]
        next # bundle handled, so skip to next fml in main loop
      }
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        stop("Each tier() argument must be a two-sided formula.", call. = FALSE)
      }

      lhs_expr <- rlang::f_lhs(fml)
      rhs_expr <- rlang::f_rhs(fml)

      vars <- .formula_vars(kn, rhs_expr)
      if (!length(vars)) {
        stop(sprintf(
          "Tier specification %s matched no variables.",
          deparse(fml)
        ), call. = FALSE)
      }
      kn <<- add_vars(kn, vars)

      if (any(!is.na(kn$vars$tier[match(vars, kn$vars$var)]))) {
        stop(sprintf(
          "Tier specification %s tries to re-assign variable(s) [%s].",
          paste(deparse(fml), collapse = ""),
          paste(vars[!is.na(kn$vars$tier[match(vars, kn$vars$var)])],
            collapse = ", "
          )
        ), call. = FALSE)
      }

      # what is on LHS?
      tier <- .parse_tier(kn, lhs_expr)

      # check if tier exists
      has_idx <- !is.na(tier$idx) &&
        tier$idx %in% kn$tiers$idx

      has_lbl <- !is.null(tier$label) &&
        !is.na(tier$label) &&
        tier$label %in% kn$tiers$label

      tier_exists <- has_idx || has_lbl

      # tier already there -> just attach variables
      if (tier_exists) {
        kn <<- add_to_tier(kn, fml)
        next
      }

      # create new tier, then attach
      after_anchor <- if (.has_any_tier(kn)) max(kn$tiers$idx) else NULL

      if (is.null(after_anchor)) {
        kn <<- add_tier(kn, !!lhs_expr)
      } else {
        kn <<- rlang::inject(add_tier(kn, !!lhs_expr, after = !!after_anchor))
      }

      kn <<- add_to_tier(kn, fml)
    }
  }

  edge_helper <- function(status, ...) {
    specs <- rlang::list2(...)
    if (!length(specs)) {
      stop(
        sprintf("%s() needs at least one two-sided formula.", status),
        call. = FALSE
      )
    }

    for (fml in specs) {
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        stop("Arguments must be two-sided formulas.", call. = FALSE)
      }

      # resolve expressions on both sides
      from_vars <- .formula_vars(kn, rlang::f_lhs(fml))
      to_vars <- .formula_vars(kn, rlang::f_rhs(fml))
      if (!is.character(from_vars) || !length(from_vars)) {
        stop(
          sprintf(
            "Edge selection `%s` matched no variables on the left-hand side of the formula.",
            paste(deparse(fml), collapse = "")
          ),
          call. = FALSE
        )
      }

      if (!is.character(to_vars) || !length(to_vars)) {
        stop(
          sprintf(
            "Edge selection `%s` matched no variables on the right-hand side of the formula.",
            paste(deparse(fml), collapse = "")
          ),
          call. = FALSE
        )
      }

      # insert every combination of from × to
      kn <<- .add_edges(kn, status, from_vars, to_vars)
    }
  }
  forbidden <- function(...) edge_helper("forbidden", ...)
  required <- function(...) edge_helper("required", ...)

  # evaluate the call list
  allowed <- c("tier", "forbidden", "required")
  for (expr in dots) {
    if (!is.call(expr) || !(as.character(expr[[1]]) %in% allowed)) {
      stop("Only tier(), forbidden(), required() calls are allowed.", call. = FALSE)
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

#' @title Add (and position) a tier
#'
#' @param .kn     A knowledge object.
#' @param tier    Bare symbol / character (label) **or** numeric literal.
#' @param before,after  Optional anchor relative to an existing tier label,
#'                tier index, or variable.  Once the knowledge object already
#'                has >= 1 tier, you must supply **exactly one** of these.
#'
#' @return Updated knowledge object.
#' @export
add_tier <- function(.kn, tier, before = NULL, after = NULL) {
  check_knowledge_obj(.kn)
  if (!missing(before) && !missing(after)) {
    stop("Cannot supply both `before` and `after`.", call. = FALSE)
  }
  # initalize tier expr and evaluate it
  tier_expr <- rlang::enexpr(tier) # keep symbols unevaluated
  tier_val <- tryCatch(
    rlang::eval_tidy(tier, env = parent.frame()),
    error = function(...) NULL
  )

  # check if tiers exist
  tiers_exist <- nrow(.kn$tiers) > 0L
  before_sup <- !missing(before)
  after_sup <- !missing(after)

  # resolve before/after
  before_expr <- rlang::enexpr(before)
  before_val <- tryCatch(
    rlang::eval_tidy(before_expr, env = parent.frame()),
    error = function(...) NULL
  )
  after_expr <- rlang::enexpr(after)
  after_val <- tryCatch(
    rlang::eval_tidy(after_expr, env = parent.frame()),
    error = function(...) NULL
  )

  # helper function to check if tier is numeric
  is_num <- function(expr, val) {
    is_num <- rlang::is_integerish(expr) && length(expr) == 1L
    is_num <- is_num || (rlang::is_integerish(val) && length(val) == 1L)
  }

  # numeric tiers
  if (is_num(tier_expr, tier_val)) {
    idx <- as.integer(tier_val)

    if (idx < 1) {
      stop("Numeric tier must be >= 1.", call. = FALSE)
    }

    # duplicate?
    if (idx %in% .kn$tiers$idx) {
      if (!missing(before) && !missing(after)) {
        stop("Numeric tier index %d already exists.", idx, call. = FALSE)
      }
      # bump up all tiers >= idx
      .kn <- .bump_tiers_up_from(.kn, idx)
    } else if (before_sup || after_sup) {
      if (before_sup + after_sup > 1L) {
        stop("Once tiers exist, supply exactly one of `before` or `after.`",
          call. = FALSE
        )
      } else if (before_sup) {
        # bump up all tiers >= idx
        if (is_num(before_expr, before_val)) {
          anchor_idx <- as.integer(before_val)
          if (anchor_idx < idx) {
            stop("`before` must be >= `tier`.", call. = FALSE)
          }
        } else {
          anchor_idx <- .tiers_from_spec(.kn, before_expr)
        }
        if (!anchor_idx %in% .kn$tiers$idx) {
          stop(
            sprintf(
              "`before` = %d does not refer to an existing tier.",
              anchor_idx
            ),
            call. = FALSE
          )
        }
        .kn <- .bump_tiers_up_from(.kn, anchor_idx)
      } else if (after_sup) {
        if (is_num(after_expr, after_val)) {
          anchor_idx <- as.integer(after_val)
          if (anchor_idx > idx) {
            stop("`after` must be <= `tier`.", call. = FALSE)
          }
        } else {
          anchor_idx <- .tiers_from_spec(.kn, after_expr)
        }
        if (!anchor_idx %in% .kn$tiers$idx) {
          stop(
            sprintf(
              "`after` = %d does not refer to an existing tier.",
              anchor_idx
            ),
            call. = FALSE
          )
        }
        .kn <- .bump_tiers_up_from(.kn, anchor_idx + 1L)
      }
    }

    # append numeric-only row (no bumping)
    .kn$tiers <- dplyr::bind_rows(
      .kn$tiers,
      tibble::tibble(idx = idx, label = NA_character_)
    ) |>
      dplyr::arrange(idx, label)

    return(.kn)
  }

  # labelled tier
  label_chr <- tryCatch(rlang::as_string(tier_expr),
    error = function(...) {
      stop("`tier` must be a numeric literal or a non-empty label.",
        call. = FALSE
      )
    }
  )

  # duplicate label?
  if (label_chr %in% .kn$tiers$label) {
    stop(sprintf("Tier label %s already exists.", label_chr),
      call. = FALSE
    )
  }

  if (tiers_exist && (before_sup + after_sup != 1L)) {
    stop("Once the knowledge object already has tiers, supply exactly one of `before` or `after`.",
      call. = FALSE
    )
  }

  # resolve anchor -> idx
  anchor_idx <- integer(0)
  if (before_sup) {
    anchor_idx <- .tiers_from_spec(.kn, rlang::enexpr(before))
  }
  if (after_sup) {
    anchor_idx <- .tiers_from_spec(.kn, rlang::enexpr(after))
  }

  if (tiers_exist && !length(anchor_idx)) {
    stop("`before` / `after` did not resolve to an existing tier/variable.",
      call. = FALSE
    )
  }
  if (length(anchor_idx) && !anchor_idx %in% .kn$tiers$idx) {
    if (before_sup) {
      stop(
        sprintf(
          "`before` = %d does not refer to an existing tier.",
          anchor_idx
        ),
        call. = FALSE
      )
    } else {
      stop(
        sprintf(
          "`after` = %d does not refer to an existing tier.",
          anchor_idx
        ),
        call. = FALSE
      )
    }
  }

  insert_idx <- if (!tiers_exist) 1L else if (before_sup) min(anchor_idx) else max(anchor_idx) + 1L

  .kn <- .bump_tiers_up_from(.kn, insert_idx)

  # register new tier
  .kn$tiers <- dplyr::bind_rows(
    .kn$tiers,
    tibble::tibble(idx = insert_idx, label = label_chr)
  ) |>
    dplyr::arrange(idx, label)

  .kn
}


#' @title Add variables to an existing tier
#'
#' @inheritParams add_tier
#' @param ...  One or more two-sided formulas `tier ~ vars`.
#'
#' @export
add_to_tier <- function(.kn, ...) {
  check_knowledge_obj(.kn)
  specs <- rlang::list2(...)
  if (!length(specs)) {
    abort("add_to_tier() needs at least one two-sided formula.")
  }

  for (fml in specs) {
    if (!rlang::is_formula(fml, lhs = TRUE)) {
      abort("Each argument must be a two-sided formula.")
    }

    lhs <- rlang::f_lhs(fml)
    rhs <- rlang::f_rhs(fml)
    tier <- .parse_tier(.kn, lhs)

    # check if tier exists
    tier_exists <- (!is.na(tier$idx) && tier$idx %in% .kn$tiers$idx) ||
      (!is.null(tier$label) && tier$label %in% .kn$tiers$label)
    if (!tier_exists) {
      stop(sprintf(
        "Tier `%s` does not exist. Create it first with add_tier().",
        rlang::as_label(lhs)
      ), call. = FALSE)
    }

    vars <- .formula_vars(.kn, rhs)
    if (!length(vars)) {
      abort(glue::glue("Specification `{deparse(rhs)}` matched no variables."))
    }

    .kn <- add_vars(.kn, vars)
    .kn$vars$tier[match(vars, .kn$vars$var)] <- tier$idx
  }

  .kn$vars <- dplyr::arrange(.kn$vars, tier, var)
  .kn
}



#' @title Add a forbidden edge to a knowledge object
#'
#' @description These edges are not allowed to be present in the final graph.
#'
#' @param .kn A `knowledge` object.
#' @param ... Either a two-sided formula (`A ~ C`) *or* `from`, `to`.
#' @export
forbid_edge <- function(.kn, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 1) {
    .edge_verb(.kn, "forbidden", dots[[1]], NULL)
  } else if (length(dots) == 2) {
    .edge_verb(.kn, "forbidden", dots[[1]], dots[[2]])
  } else {
    stop("forbid_edge() takes either 1 or 2 edge specifications.", call. = FALSE)
  }
}

#' @title Add a forbidden edge to a knowledge object
#'
#' @description These edges are not allowed to be present in the final graph.
#'
#' @inheritParams forbid_edge
#' @export
require_edge <- function(.kn, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 1) {
    .edge_verb(.kn, "required", dots[[1]], NULL)
  } else if (length(dots) == 2) {
    .edge_verb(.kn, "required", dots[[1]], dots[[2]])
  } else {
    stop("require_edge() takes either 1 or 2 edge specifications.", call. = FALSE)
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
    ord <- order(unname(x$tier_labels))
    labs <- names(x$tier_labels)[ord]
    idx <- unname(x$tier_labels)[ord]

    cli::cat_line("Tier labels:")
    cli::cat_line(paste0("  * ", labs, " -> ", idx, collapse = "\n"))
  }

  cli::cat_line(cli::style_bold("Variables:"), nrow(x$vars))
  if (nrow(x$vars)) print(x$vars, n = Inf)

  cli::cat_line(cli::style_bold("Edges:"), nrow(x$edges))
  if (nrow(x$edges)) print(x$edges, n = 10)

  invisible(x)
}

# ────────────────────────────── Manipulation ──────────────────────────────────

#' @title Merge two `knowledge` objects
#' @exportS3Method "+" knowledge
`+.knowledge` <- function(e1, e2) {
  stopifnot(inherits(e1, "knowledge"), inherits(e2, "knowledge"))

  vars_all <- unique(c(e1$vars$var, e2$vars$var))
  out <- .new_knowledge(vars_all)

  vtiers <- dplyr::bind_rows(e2$vars, e1$vars) |>
    dplyr::group_by(var) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  out$vars$tier <- vtiers$tier[match(out$vars$var, vtiers$var)]

  tier_tbl <- dplyr::bind_rows(
    e1$tiers,
    e2$tiers
  ) |>
    dplyr::distinct(idx, label, .keep_all = TRUE) |>
    dplyr::arrange(idx, label)

  # detect: same idx -> different labels
  same_idx_diff_labels <- tier_tbl |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(idx) |>
    dplyr::filter(dplyr::n_distinct(label) > 1) |>
    dplyr::pull(idx) |>
    unique()

  # detect: same label -> different idx
  same_label_diff_idxs <- tier_tbl |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(label) |>
    dplyr::filter(dplyr::n_distinct(idx) > 1) |>
    dplyr::pull(label) |>
    unique()

  if (length(same_idx_diff_labels) || length(same_label_diff_idxs)) {
    msg <- c(
      if (length(same_idx_diff_labels)) {
        sprintf(
          "idx %s has conflicting labels",
          paste(same_idx_diff_labels, collapse = ", ")
        )
      },
      if (length(same_label_diff_idxs)) {
        sprintf(
          "label(s) %s map to different indices",
          paste(same_label_diff_idxs, collapse = ", ")
        )
      }
    )
    stop("Cannot merge knowledge objects:\n  * ",
      paste(msg, collapse = "\n  * "),
      call. = FALSE
    )
  }

  # ensure numeric-only tiers present for all idx referenced by vars
  missing_idx <- setdiff(
    na.omit(unique(out$vars$tier)),
    tier_tbl$idx
  )
  if (length(missing_idx)) {
    tier_tbl <- dplyr::bind_rows(
      tier_tbl,
      tibble::tibble(idx = missing_idx, label = NA_character_)
    )
  }
  # sort tiers
  out$tiers <- dplyr::arrange(tier_tbl, idx, label)

  # ensure all tiers are present in the knowledge object
  out$edges <- dplyr::distinct(dplyr::bind_rows(e1$edges, e2$edges))
  out <- .update_edge_tiers(out)

  # validate
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
    stop("Input must be a knowledge instance.", call. = FALSE)
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
    function(status, from, to, ...) {
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
#'   • **fixedGaps**  – **forbidden** edges (should be going both ways))
#'   • **fixedEdges** – **required** edges (should be going both ways))
#'
#' We therefore throw an error if any edges relations are not symmetrical.
#'
#' @param .kn A `knowledge` object.
#' @param labels Character vector of variable names in the *exact* order of
#'               your data matrix / data frame.
#' @return A list with elements `fixedGaps` and `fixedEdges`.
#' @export
as_pcalg_constraints <- function(.kn, labels, directed_as_undirected = FALSE) {
  check_knowledge_obj(.kn)

  if (any(!is.na(.kn$vars$tier))) {
    stop(
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
  idx <- rlang::set_names(seq_along(labels), labels)

  # find the relations that are non-symmetric
  if (!directed_as_undirected) {
    bad_edges <- .kn$edges |>
      # keep only those edges for which the reversed pair is NOT present
      dplyr::anti_join(
        .kn$edges,
        by = c("from" = "to", "to" = "from")
      ) |>
      dplyr::mutate(
        desc = paste0(from, " --> ", to)
      ) |>
      dplyr::pull(desc)

    if (length(bad_edges)) {
      stop(
        "pcalg does not support asymmetric edges.\n",
        "The following do not have a symmetrical counterpart:\n",
        paste0("  * ", bad_edges, collapse = "\n"),
        call. = FALSE
      )
    }
  }

  # Forbidden edges
  forb <- dplyr::filter(.kn$edges, status == "forbidden")
  if (nrow(forb)) {
    for (k in seq_len(nrow(forb))) {
      i <- idx[[forb$from[k]]]
      j <- idx[[forb$to[k]]]
      if (is.na(i) || is.na(j)) {
        stop("Forbidden edge refers to unknown variable(s).", call. = FALSE)
      }
      if (directed_as_undirected) {
        fixedGaps[j, i] <- TRUE
      }
      fixedGaps[i, j] <- TRUE
    }
  }

  # Required edges
  req <- dplyr::filter(.kn$edges, status == "required")
  if (nrow(req)) {
    for (k in seq_len(nrow(req))) {
      i <- idx[[req$from[k]]]
      j <- idx[[req$to[k]]]
      if (is.na(i) || is.na(j)) {
        stop("Required edge refers to unknown variable(s).", call. = FALSE)
      }
      if (directed_as_undirected) {
        fixedEdges[j, i] <- TRUE
      }
      fixedEdges[i, j] <- TRUE
    }
  }

  list(fixedGaps = fixedGaps, fixedEdges = fixedEdges)
}

#' @title Forbid all “uphill” edges implied by tiers
#' @description
#' Given a `knowledge` object with variables already assigned to tiers,
#' forbids every directed edge that runs from a higher-numbered tier down
#' into a lower-numbered tier.
#' @param .kn A `knowledge` object.
#' @return The same `knowledge` object with new forbidden edges added.
#' @export
forbid_tier_violations <- function(.kn) {
  check_knowledge_obj(.kn)

  # Rename so we can cross without duplicate names
  from <- .kn$vars %>%
    dplyr::rename(var_from = var, tier_from = tier)
  to <- .kn$vars %>%
    dplyr::rename(var_to = var, tier_to = tier)

  # Every possible (from, to) pair
  bad <- tidyr::crossing(from, to) %>%
    # Filer tier violations
    dplyr::filter(tier_from > tier_to)

  if (nrow(bad)) {
    .kn <- .add_edges(
      .kn,
      status    = "forbidden",
      from      = bad$var_from,
      to        = bad$var_to
    )
  }
  .kn
}


#' @title Generate a Bundle of Tier–Variable Formulas
#'
#' @description
#' Quickly create a series of two‐sided formulas for use with \code{tier()},
#' where each formula maps a numeric tier index to a tidyselect specification
#' that contains the placeholder \code{i}.  The placeholder \code{i} is replaced
#' by each element of \code{tiers} in turn, allowing you to write a single
#' template rather than many nearly identical formulas.
#'
#' @param tiers
#'   An integer vector of tier indices (each >= 1). These will appear as the
#'   left‐hand sides of the generated formulas.
#'
#' @param vars
#'   A tidyselect specification (unevaluated) that *must* contain the special
#'   placeholder \code{i}, either as the symbol \code{i} or inside a string
#'   like \code{"…{i}…"}.  For each value of \code{i} in \code{tiers}, that
#'   placeholder will be substituted and the resulting call used as the
#'   right‐hand side of a formula.
#'
#' @return
#'   A list of two‐sided formulas, each of class \code{"tier_bundle"}.
#'   You can pass this list directly to \code{tier()} (which will expand it
#'   automatically).
#'
#' @examples
#' # Suppose your data frame has columns X_1, X_2, X_3, X_4
#' # Create formulas 1 ~ ends_with("1"), 2 ~ ends_with("2"), etc.
#' formulas <- seq_tiers(1:4, ends_with("_{i}"))
#' tier(
#'   # this expands into 1 ~ ends_with("_1"), 2 ~ ends_with("_2"), …
#'   formulas
#' )
#'
#' # You can also use matches() with a custom pattern
#' tier(
#'   seq_tiers(4:9, matches("Var{i}th$"))
#' )
#'
#' @seealso
#' \code{\link{tier}}, for turning these formulas into actual tiers.
#'
#' @export
seq_tiers <- function(tiers, vars) {
  stopifnot(is.numeric(tiers), all(tiers >= 1L))

  vars_expr <- rlang::enexpr(vars)

  # guard: placeholder must be present
  if (!rlang::is_call(vars_expr) &&
    !identical(vars_expr, quote(i)) &&
    !grepl("{i}", deparse(vars_expr), fixed = TRUE)) {
    stop("`vars` must contain the placeholder `i`.", call. = FALSE)
  }

  # recursively substitute `i` or "{i}" helper
  replace_i <- function(expr, i_chr) {
    switch(typeof(expr),
      "language"  = as.call(lapply(as.list(expr), replace_i, i_chr)),
      "symbol"    = if (identical(expr, quote(i))) rlang::expr(!!i_chr) else expr,
      "character" = rlang::expr(!!gsub("{i}", i_chr, expr, fixed = TRUE)),
      expr
    )
  }

  # build formulas helper
  build_formula <- function(i) {
    rhs <- replace_i(vars_expr, as.character(i))
    rlang::new_formula(i, rhs, env = rlang::empty_env())
  }
  # create the formulas
  structure(lapply(tiers, build_formula), class = "tier_bundle")
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
      tiers = tibble::tibble(
        idx   = integer(),
        label = character()
      ),
      edges = tibble::tibble(
        status     = character(),
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

# ─────────────────────────── Validation helpers  ──────────────────────────────
#' @title Validate that no edge runs from higher tier to lower tier
#'
#' @param edges_df A data frame with columns `status`, `from`,
#' `to`, `tier_from`, and `tier_to`.
#' @keywords internal
.validate_tier_rule <- function(edges_df) {
  tier_violations <- dplyr::filter(
    edges_df,
    !is.na(tier_from),
    !is.na(tier_to),
    status != "forbidden", # forbidden can't violate tiers
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
#' @param edges_df A data frame with columns `status`, `from`,
#' `to`, `tier_from`, and `tier_to`.
#' @keywords internal
.validate_forbidden_required <- function(edges_df) {
  # look for groups where the same (from, to) has both statuses
  clashes <- edges_df |>
    dplyr::group_by(from, to) |>
    dplyr::filter(all(c("forbidden", "required") %in% status)) |>
    dplyr::ungroup() |>
    dplyr::distinct(from, to) # one row per conflicting edge
  if (nrow(clashes)) {
    stop(
      "Edge(s) appear as both forbidden and required: ",
      paste0(clashes$from, " --> ", clashes$to,
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
#' numeric tier index** after the merge (e.g. "January -> 1" and "Monday -> 1").
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
#' * __Brand-new label__ (any other symbol/character) ->  marks it for creation
#'   by returning `NA` together with the textual label.
#'
#' @param .kn A `knowledge` object.
#' @param tier A symbol, character string, or single positive integer.
#'
#' @return A named list with components
#'   * `idx`   `integer(1)` — resolved tier index (or `NA` if the label is new)
#'   * `label` `character(1)`|`NULL` — the label if one was supplied
#'
#' @keywords internal
.parse_tier <- function(.kn, tier) {
  # if tier is a symbol, evaluate it in the parent frame
  tier_val <- tryCatch(
    rlang::eval_tidy(tier, env = parent.frame()),
    error = function(...) NULL
  )
  if (!is.null(tier_val)) tier <- tier_val
  if (rlang::is_integerish(tier) && length(tier) == 1) {
    if (tier < 1) {
      stop("Numeric tier must be >= 1.", call. = FALSE)
    }
    tier_idx <- as.integer(tier)
    # does that index already have a label?
    lbl <- dplyr::filter(.kn$tiers, .data$idx == tier_idx)$label
    lbl <- if (length(lbl)) lbl else NULL

    return(list(idx = tier_idx, label = lbl, kn = .kn))
  }

  # symbol / character
  tier_label <- rlang::as_string(tier)
  if (!nzchar(tier_label)) {
    stop("`tier` must be a number >= 1 or a non-empty label.", call. = FALSE)
  }

  row <- dplyr::filter(.kn$tiers, .data$label == tier_label)

  list(
    idx   = if (nrow(row)) row$idx else NA_integer_,
    label = tier_label
  )
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

  # explode the expression into individual parts
  parts <- rlang::exprs(!!x)

  # unwrap a lone c(...) call
  if (length(parts) == 1L && rlang::is_call(parts[[1]], "c")) {
    parts <- rlang::call_args(parts[[1]])
  }

  # map each part -> integer tier index
  as.integer(unlist(lapply(parts, function(elm) {
    # numeric literal
    if (rlang::is_integerish(elm)) {
      return(as.integer(elm))
    }


    # symbol / character
    if (rlang::is_symbol(elm) || is.character(elm)) {
      token <- rlang::as_string(elm)

      # token is a tier label
      hit <- which(!is.na(.kn$tiers$label) & .kn$tiers$label == token)
      if (length(hit)) {
        return(.kn$tiers$idx[hit])
      }

      # token is a variable name
      if (token %in% .kn$vars$var) {
        idx <- .kn$vars$tier[.kn$vars$var == token]
        if (is.na(idx)) {
          stop(sprintf(
            "Variable `%s` has no tier; cannot use in `before/after`.",
            token
          ), call. = FALSE)
        }
        return(idx)
      }
    }

    # otherwise
    stop(sprintf(
      "`%s` is not a tier label, index, or variable.",
      rlang::as_label(elm)
    ), call. = FALSE)
  })))
}



#' @title Shift all tiers **>=** a position *up* by one
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
  # bump variables
  .kn$vars$tier <- ifelse(
    !is.na(.kn$vars$tier) & .kn$vars$tier >= insert_idx,
    .kn$vars$tier + 1L,
    .kn$vars$tier
  )

  # bump only labelled tiers
  bump_row <- .kn$tiers$idx >= insert_idx & !is.na(.kn$tiers$label)
  .kn$tiers$idx[bump_row] <- .kn$tiers$idx[bump_row] + 1L
  .kn$tiers <- dplyr::arrange(.kn$tiers, idx, label)

  # refresh edge annotation
  .kn <- .update_edge_tiers(.kn)
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

#' @title Find row in the tiers dataframe matching a given index or label
#'
#' @param .kn A `knowledge` object.
#' @param idx A numeric index or `NULL`.
#' @param label A character label or `NULL`.
#'
.tier_row <- function(.kn, idx = NULL, label = NULL) {
  dplyr::filter(
    .kn$tiers,
    (is.null(idx) || idx == .data$idx) &
      (is.null(label) || label == .data$label)
  )
}

#' @title Check if knowledge object has a tier
#'
#' @description Used internally in `add_tier()`
#' @keywords internal
.has_any_tier <- function(.kn) nrow(.kn$tiers) > 0L

# ───────────────────────────── Edge helpers  ──────────────────────────────────
#' @title Add one or many edges to a knowledge object
#'
#' @param .kn A `knowledge` object.
#' @param status A string, either "forbidden" or "required".
#' @param from A tidyselect specification or character vector of variable names.
#' @param to A tidyselect specification or character vector of variable names.
#' @keywords internal
.add_edges <- function(.kn, status, from, to) {
  # Resolve `from` / `to` specs into character vectors of variable names
  from_chr <- .vars_from_spec(.kn, {{ from }})
  to_chr <- .vars_from_spec(.kn, {{ to }})

  # Ensure all endpoint variables exist in `.kn$vars`
  .kn <- add_vars(.kn, unique(c(from_chr, to_chr)))

  # Cartesian product -> one row per directed edge, then annotate
  block <- tidyr::crossing(from = from_chr, to = to_chr) |>
    dplyr::mutate(
      status    = status,
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
#' @keywords internal
.edge_verb <- function(.kn, status,
                       from_quo, to_quo = NULL) {
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
  .add_edges(.kn, status, from_vars, to_vars)
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
    # (e.g. `starts_with("V")` -> `c("V1", "V2", ...)`)
    # This will throw an error if the spec is not valid
    names(tidyselect::eval_select(rlang::enquo(all_of(spec)), lookup)),
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

# ──────────────────────────────── Imports ─────────────────────────────────────

#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows distinct group_by slice ungroup filter mutate pull
#' @importFrom tidyselect eval_select everything starts_with ends_with
#' @importFrom rlang enquo eval_tidy set_names is_formula f_lhs f_rhs as_name !!
#' @importFrom rlang is_integerish as_string list2 inject new_formula empty_env
#' @importFrom purrr pwalk
#' @importFrom cli cat_rule cat_line style_bold
NULL
