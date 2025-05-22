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
  df <- NULL

  if (length(dots)) {
    first <- tryCatch(
      eval(dots[[1]], parent.frame()),
      error = function(e) NULL
    )
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

    ## ────────────────────────── numeric-vector shortcut ──────────────────────
    if (length(specs) == 1L &&
      is.numeric(specs[[1]]) &&
      is.atomic(specs[[1]])) {
      vec_num <- specs[[1]]
      vars <- kn$vars$var

      if (!length(vars)) {
        stop(
          "Using tier(<numeric vector>) needs the data-frame columns first.",
          call. = FALSE
        )
      }
      if (length(vec_num) != length(vars)) {
        stop(
          "Tier vector length (", length(vec_num),
          ") must equal number of variables (", length(vars), ").",
          call. = FALSE
        )
      }

      existing_num <- suppressWarnings(as.numeric(kn$tiers$label))
      existing_num <- existing_num[!is.na(existing_num)]
      dup <- intersect(vec_num, existing_num)
      if (length(dup)) {
        stop(
          sprintf("Tier index(es) %s already exist.", paste(dup, collapse = ", ")),
          call. = FALSE
        )
      }

      vec_lab <- as.character(vec_num) # treat as labels
      new_lbl <- setdiff(unique(vec_lab), kn$tiers$label)

      if (length(new_lbl)) {
        kn$tiers <<- dplyr::bind_rows(
          kn$tiers,
          tibble::tibble(label = new_lbl)
        ) |>
          dplyr::arrange(suppressWarnings(as.numeric(label)), label)
      }

      kn$vars <<- dplyr::mutate(kn$vars, tier = vec_lab)
      return(kn)
    }

    if (!length(specs)) {
      stop("tier() needs at least one two-sided formula.", call. = FALSE)
    }

    ## ───────────────────────────── main loop ─────────────────────────────────
    for (fml in specs) {
      # ---------- tier_bundle ----------
      if (inherits(fml, "tier_bundle")) {
        tier_vec <- character(length(kn$vars$var))

        for (g in fml) {
          lbl <- as.character(rlang::f_lhs(g))
          rhs <- rlang::f_rhs(g)

          pos <- tidyselect::eval_select(
            rhs,
            setNames(seq_along(kn$vars$var), kn$vars$var)
          )

          if (!length(pos)) {
            stop("Pattern ", deparse(rhs), " matched no variables.",
              call. = FALSE
            )
          }
          if (any(tier_vec[pos] != "")) {
            dup <- kn$vars$var[pos[tier_vec[pos] != ""]]
            stop("Some variables matched by two patterns: ",
              paste(dup, collapse = ", "),
              call. = FALSE
            )
          }
          tier_vec[pos] <- lbl
        }

        # ensure catalog contains every referenced label
        miss <- setdiff(unique(tier_vec[tier_vec != ""]), kn$tiers$label)
        if (length(miss)) {
          kn$tiers <<- dplyr::bind_rows(
            kn$tiers,
            tibble::tibble(label = miss)
          )
        }

        kn$vars$tier[tier_vec != ""] <<- tier_vec[tier_vec != ""]
        next
      }

      # ---------- ordinary two-sided formula ----------
      if (!rlang::is_formula(fml, lhs = TRUE)) {
        stop("Each tier() argument must be a two-sided formula.", call. = FALSE)
      }

      lhs_expr <- rlang::f_lhs(fml)
      rhs_expr <- rlang::f_rhs(fml)

      # derive a single-string label:
      tier_val <- tryCatch(
        rlang::eval_tidy(lhs_expr, env = parent.frame()),
        error = function(e) NULL
      )

      if (is.character(tier_val) && length(tier_val) == 1L && nzchar(tier_val)) {
        tier_label <- tier_val
      } else if (is.numeric(tier_val) && length(tier_val) == 1L) {
        tier_label <- as.character(tier_val)
      } else {
        tier_label <- rlang::as_label(lhs_expr)
      }

      vars <- .formula_vars(kn, rhs_expr)
      if (!length(vars)) {
        stop(sprintf(
          "Tier specification %s matched no variables.",
          deparse(fml)
        ), call. = FALSE)
      }
      kn <<- add_vars(kn, vars)

      # guard against re-assigning a var that is already in another tier
      clash <- kn$vars$tier[match(vars, kn$vars$var)]
      if (any(!is.na(clash) & clash != tier_label)) {
        bad <- vars[!is.na(clash) & clash != tier_label]
        stop(sprintf(
          "Tier specification %s tries to re-assign variable(s) [%s] to a new tier.",
          paste(deparse(fml), collapse = ""),
          paste(bad, collapse = ", ")
        ), call. = FALSE)
      }

      if (tier_label %in% kn$tiers$label) {
        kn <<- add_to_tier(kn, fml) # already exists → just attach
        next
      }

      # create new tier after the current last one
      after_anchor <- if (nrow(kn$tiers)) tail(kn$tiers$label, 1) else NULL

      if (is.null(after_anchor)) {
        kn <<- add_tier(kn, !!lhs_expr)
      } else {
        kn <<- rlang::inject(add_tier(kn, !!lhs_expr, after = !!after_anchor))
      }

      kn <<- add_to_tier(kn, fml)
    }

    kn
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
      "\nThey are not present in the data frame provided to this knowledge object.",
      call. = FALSE
    )
  }

  if (length(missing)) {
    new_rows <- tibble::tibble(var = missing, tier = NA_character_)
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
  before_sup <- !missing(before)
  after_sup <- !missing(after)

  if (before_sup && after_sup) {
    stop("Cannot supply both `before` and `after`.", call. = FALSE)
  }

  # capture the new label
  tier_expr <- rlang::enexpr(tier)

  if (length(tier_expr) != 1L) {
    stop("`tier` must be a single non-empty label or a non-negative numeric literal.", call. = FALSE)
  }
  tier_val <- tryCatch(
    rlang::eval_tidy(tier_expr, env = parent.frame()),
    error = function(e) NULL
  )

  if (!is.symbol(tier_expr)) {
    if (is.null(tier_expr) || is.na(tier_expr)) {
      stop("`tier` must be a single non-empty label or a non-negative numeric literal.", call. = FALSE)
    }
  }

  if (is.character(tier_val) && length(tier_val) == 1L) {
    label <- tier_val
  } else if (is.numeric(tier_val) && length(tier_val) == 1L) {
    label <- as.character(tier_val)
  } else {
    label <- rlang::as_label(tier_expr)
  }

  if (length(label) != 1L || is.na(label) || !nzchar(label)) {
    stop("`tier` must be a non-empty label.", call. = FALSE)
  }

  # duplicate?
  if (label %in% .kn$tiers$label) {
    stop(sprintf("Tier label `%s` already exists.", label), call. = FALSE)
  }

  tiers_exist <- nrow(.kn$tiers) > 0L

  # no tiers yet
  if (!tiers_exist) {
    if (before_sup || after_sup) {
      stop(
        "`before`/`after` cannot be used when there are no existing tiers.",
        call. = FALSE
      )
    }
    .kn$tiers <- dplyr::bind_rows(.kn$tiers, tibble::tibble(label = label))
    return(.kn)
  }

  # tiers exist

  # must supply exactly one of before/after
  if ((before_sup + after_sup) != 1L) {
    stop(
      "Once the knowledge object already has tiers, supply exactly one of ",
      "`before` or `after`.",
      call. = FALSE
    )
  }

  # resolve anchor to a label string
  anchor_lbl <- if (before_sup) {
    as.character(rlang::enexpr(before))
  } else {
    as.character(rlang::enexpr(after))
  }

  pos <- match(anchor_lbl, .kn$tiers$label)
  if (is.na(pos)) {
    stop(sprintf("`%s` does not refer to an existing tier.", anchor_lbl),
      call. = FALSE
    )
  }

  insert_at <- if (before_sup) pos else pos + 1L

  # build new tiers in three parts
  head_part <- dplyr::slice(.kn$tiers, seq_len(insert_at - 1L))

  tail_part <- if (insert_at <= nrow(.kn$tiers)) {
    dplyr::slice(.kn$tiers, insert_at:nrow(.kn$tiers))
  } else {
    .kn$tiers[0, ] # empty tibble w/ same columns
  }

  .kn$tiers <- dplyr::bind_rows(
    head_part,
    tibble::tibble(label = label),
    tail_part
  )
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
    stop("add_to_tier() needs at least one two-sided formula.")
  }

  for (fml in specs) {
    if (!rlang::is_formula(fml, lhs = TRUE)) {
      stop("Each argument must be a two-sided formula.")
    }

    lhs_expr <- rlang::f_lhs(fml)
    rhs_expr <- rlang::f_rhs(fml)
    tier_label <- as.character(lhs_expr)

    # tier must already exist
    if (!tier_label %in% .kn$tiers$label) {
      stop(
        sprintf(
          "Tier `%s` does not exist. Create it first with add_tier().",
          tier_label
        ),
        call. = FALSE
      )
    }

    # resolve variables on the RHS
    vars <- .formula_vars(.kn, rhs_expr)
    if (!length(vars)) {
      stop(glue::glue("Specification `{deparse(rhs_expr)}` matched no variables."))
    }

    # detect variables already assigned to a different tier
    current <- .kn$vars$tier[match(vars, .kn$vars$var)]
    clash <- !is.na(current) & current != tier_label
    if (any(clash)) {
      bad <- vars[clash]
      stop(
        sprintf(
          "Cannot reassign variable(s) [%s] to tier `%s` using add_to_tier().\n",
          paste(bad, collapse = ", "), tier_label
        ),
        call. = FALSE
      )
    }

    # register variables and attach the tier label
    .kn <- add_vars(.kn, vars)
    .kn$vars$tier[match(vars, .kn$vars$var)] <- tier_label
  }

  # update tier_from and tier_to in edges
  if (nrow(.kn$edges)) {
    idx_from <- match(.kn$edges$from, .kn$vars$var)
    idx_to <- match(.kn$edges$to, .kn$vars$var)

    .kn$edges$tier_from <- .kn$vars$tier[idx_from]
    .kn$edges$tier_to <- .kn$vars$tier[idx_to]

    # check if we violate tier order
    .validate_tier_rule(.kn$edges, .kn$tiers)
  }

  # tidy variable table: order by tier rank, then name
  rank <- match(.kn$vars$tier, .kn$tiers$label)
  .kn$vars <- dplyr::arrange(.kn$vars, rank, var)

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
#' Required edges can only be in one direction. That is, you cannot both require
#' V1 --> V2 and V2 --> V1.
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
`+.knowledge` <- function(kn1, kn2) {
  stopifnot(inherits(kn1, "knowledge"), inherits(kn2, "knowledge"))

  # combine
  vars_all <- unique(c(kn1$vars$var, kn2$vars$var))
  out <- .new_knowledge(vars_all)

  # var tiers
  vtiers <- dplyr::bind_rows(kn1$vars, kn2$vars) |>
    dplyr::group_by(var) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  # merge vars
  out$vars$tier <- vtiers$tier[match(out$vars$var, vtiers$var)]

  # merge tier labels, preserving kn1 order then any new from kn2
  all_labels <- unique(c(kn1$tiers$label, kn2$tiers$label))
  out$tiers <- tibble::tibble(label = all_labels)

  # merge edges (status, from, to, tier_from, tier_to are all character)
  out$edges <- dplyr::distinct(dplyr::bind_rows(kn1$edges, kn2$edges)) |>
    dplyr::mutate(
      tier_from = out$vars$tier[match(from, out$vars$var)],
      tier_to   = out$vars$tier[match(to, out$vars$var)]
    )

  # validate
  .validate_forbidden_required(out$edges)
  .validate_tier_rule(out$edges, out$tiers)

  out
}

#' @title Reorder all tiers at once
#'
#' @param .kn      A `knowledge` object.
#' @param order    A vector that lists *every* tier exactly once, either by
#'                 label (default) or by numeric index (`by_index = TRUE`).
#'                 Be careful if you have numeric tier labels.
#' @param by_index If `TRUE`, treat `order` as the positions instead of
#'                 labels. Defaults to `FALSE`.
#'
#' @return The same `knowledge` object with tiers rearranged.
#' @export
reorder_tiers <- function(.kn, order, by_index = FALSE) {
  check_knowledge_obj(.kn)

  current <- .kn$tiers$label
  n <- length(current)

  # helper function to convert a label expression to a string
  as_label1 <- function(expr) {
    if (rlang::is_symbol(expr)) {
      return(as.character(expr))
    }
    if (rlang::is_character(expr)) {
      return(rlang::as_string(expr))
    }
    if (rlang::is_atomic(expr) && length(expr) == 1L) {
      val <- rlang::eval_tidy(expr, env = parent.frame())
      if (is.numeric(val)) {
        return(as.character(val))
      }
      if (is.character(val) && nzchar(val)) {
        return(val)
      }
    }
    stop("`order` contains an unsupported element: ", rlang::expr_text(expr),
      call. = FALSE
    )
  }

  # turn input into character label
  if (by_index) {
    idx <- rlang::eval_tidy(rlang::enexpr(order), env = parent.frame())
    if (!is.numeric(idx) || length(idx) != n || !setequal(idx, seq_len(n))) {
      stop("`order` must be a permutation of 1:", n, " when `by_index = TRUE`.",
        call. = FALSE
      )
    }
    labels <- current[idx]
  } else {
    expr <- rlang::enexpr(order)

    # unwrap literal c(...) call, and get a list of expressions
    parts <- if (rlang::is_call(expr, "c")) rlang::call_args(expr) else list(expr)

    labels <- vapply(parts, as_label1, character(1))
    labels <- unname(labels)

    if (length(labels) != n || !setequal(labels, current)) {
      stop("`order` must list every existing tier exactly once.", call. = FALSE)
    }
  }

  # apply new order
  .kn$tiers <- tibble::tibble(label = labels)

  # validate
  .validate_tier_rule(.kn$edges, .kn$tiers)
  .validate_forbidden_required(.kn$edges)

  # return
  .kn
}


#' @title Move one tier before / after another
#'
#' @inheritParams reorder_tiers
#' @param tier   The tier to move (label or index, honouring `by_index`).
#' @param before,after Exactly one of these must be supplied and must identify
#'                     another existing tier.
#'
#' @return The updated `knowledge` object.
#' @export
reposition_tier <- function(.kn, tier, before = NULL, after = NULL, by_index = FALSE) {
  check_knowledge_obj(.kn)
  if (!xor(missing(before), missing(after))) {
    stop("Supply exactly one of `before` or `after`.", call. = FALSE)
  }

  current <- .kn$tiers$label

  resolve_label <- function(expr) {
    if (by_index) {
      idx <- rlang::eval_tidy(expr, env = parent.frame())
      if (!is.numeric(idx) || length(idx) != 1L) {
        stop("When `by_index = TRUE`, tier references must be length-1 numeric.")
      }
      return(current[idx])
    }

    val <- tryCatch(rlang::eval_tidy(expr, env = parent.frame()),
      error = function(e) NULL
    )

    if (is.character(val) && length(val) == 1L && nzchar(val)) {
      return(val)
    }
    if (is.numeric(val) && length(val) == 1L) {
      return(as.character(val))
    }
    if (rlang::is_symbol(expr)) {
      return(as.character(expr))
    }
    stop("Tier reference ", rlang::expr_text(expr), " is invalid.")
  }

  tier_lbl <- resolve_label(rlang::enexpr(tier))
  anchor_lbl <- resolve_label(if (missing(before)) {
    rlang::enexpr(after)
  } else {
    rlang::enexpr(before)
  })

  if (!tier_lbl %in% current) stop("Tier `", tier_lbl, "` does not exist.")
  if (!anchor_lbl %in% current) stop("Anchor tier `", anchor_lbl, "` does not exist.")
  if (tier_lbl == anchor_lbl) {
    return(.kn)
  } # nothing to do

  new_order <- setdiff(current, tier_lbl) # drop, then re-insert
  pos <- match(anchor_lbl, new_order)
  insert_at <- if (missing(before)) pos + 1L else pos
  new_order <- append(new_order, tier_lbl, after = insert_at - 1L)
  reorder_tiers(.kn, c(!!!new_order))
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
    stop("Package 'rJava' is required for as_tetrad_knowledge().")
  }

  j <- rJava::.jnew("edu/cmu/tetrad/data/Knowledge")

  # attach every variable that has a tier label
  purrr::pwalk(
    list(.kn$vars$var, .kn$vars$tier),
    function(v, t) {
      if (!is.na(t)) {
        idx <- match(t, .kn$tiers$label) # row position = tier rank
        j$addToTier(as.integer(idx), v)
      }
    }
  )

  # transfer forbidden / required edges
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
      vars = tibble::tibble(var = vars, tier = NA_character_),
      tiers = tibble::tibble(label = character()),
      edges = tibble::tibble(
        status     = character(),
        from       = character(),
        to         = character(),
        tier_from  = character(),
        tier_to    = character()
      ),
      frozen = frozen
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
.validate_tier_rule <- function(edges_df, tiers) {
  rank <- function(lbl) match(lbl, tiers$label)

  bad <- dplyr::filter(
    edges_df,
    !is.na(tier_from), !is.na(tier_to),
    status != "forbidden",
    rank(tier_from) > rank(tier_to)
  )
  if (nrow(bad)) {
    stop("Edge(s) violate tier ordering: ",
      paste(bad$from, "-->", bad$to, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @title Detect inconsistent edge declarations
#'
#' @description Throws an error if any edges are declared as both
#' `forbidden` and `required`, or if a required edge is declared in both
#' directions.
#'
#' @param edges_df Data frame with columns `status`, `from`, `to`,
#'   `tier_from`, and `tier_to`.
#' @keywords internal
.validate_forbidden_required <- function(edges_df) {
  # same ordered edge both forbidden and required
  clash_fr <- edges_df |>
    dplyr::group_by(from, to) |>
    dplyr::filter(all(c("forbidden", "required") %in% status)) |>
    dplyr::ungroup() |>
    dplyr::distinct(from, to)

  if (nrow(clash_fr)) {
    stop(
      "Edge(s) appear as both forbidden and required: ",
      paste0(clash_fr$from, " --> ", clash_fr$to, collapse = ", "),
      call. = FALSE
    )
  }

  # required edge in both directions
  req <- dplyr::filter(edges_df, status == "required")

  if (nrow(req) > 1) {
    # normalise each pair to an unordered signature "A||B"
    sig <- paste(pmin(req$from, req$to), pmax(req$from, req$to), sep = "||")
    dup <- sig[duplicated(sig)]

    if (length(dup)) {
      offending <- unique(dup)
      pretty <- gsub("\\|\\|", " <-> ", offending) # A <-> B
      stop(
        "Edge(s) required in both directions: ",
        paste(pretty, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # no problems
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
  label <- as.character(tier)

  if (!nzchar(label)) {
    stop("`tier` must be a non-empty label.", call. = FALSE)
  }

  list(
    idx   = match(label, .kn$tiers$label), # NA if new
    label = label
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
    return(character())
  }

  parts <- rlang::exprs(!!x)
  if (length(parts) == 1L && rlang::is_call(parts[[1]], "c")) {
    parts <- rlang::call_args(parts[[1]])
  }

  vapply(parts, function(elm) {
    token <- as.character(elm)
    if (token %in% .kn$tiers$label) {
      return(token)
    }
    if (token %in% .kn$vars$var) {
      lbl <- .kn$vars$tier[.kn$vars$var == token]
      if (is.na(lbl)) {
        stop(sprintf("Variable `%s` has no tier.", token), call. = FALSE)
      }
      return(lbl)
    }
    stop(sprintf("`%s` is not a tier label or variable.", token), call. = FALSE)
  }, character(1))
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

  # stop if any new edge violates the tier rule
  .validate_tier_rule(block, .kn$tiers)

  # stop if any new edge violates the forbidden/required rule
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
#' @param from A tidyselect specification or a variable name string or symbol or formula.
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
    if (!length(from_vars) && rlang::is_symbol(from_quo)) {
      from_vars <- as.character(from_quo)
    }
    print(from_vars)
    to_vars <- .vars_from_spec(.kn, !!to_quo)
    if (!length(to_vars) && rlang::is_symbol(to_quo)) {
      to_vars <- as.character(to_quo)
    }
    print(to_vars)
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
