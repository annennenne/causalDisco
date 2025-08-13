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
#' @param ... Either
#'  - a single **data frame** (first argument) whose column names initialise the
#'    variable set and freeze the knowledge object, followed by
#'  - zero or more calls to the mini‑DSL:
#'    `tier()`, `forbidden()`, `required()`, or `exogenous()` (aliases: `exo()`,
#'    `root()`).
#'
#'  For `tier()` you may pass one or more two‑sided **formulas**
#'  (`tier(1 ~ x + y)`, `tier("baseline" ~ starts_with("V"))`) or a single
#'  numeric vector shortcut matching the number of variables to set tiers by
#'  index. `forbidden()`/`required()` take one or more two‑sided formulas
#'  (`from ~ to`), where each side can use tidyselect semantics to select
#'  multiple variables. `exogenous()`/`exo()`/`root()` take variable selectors
#'  (names or tidyselect), possibly multiple. Arguments are evaluated in order;
#'  only these calls are allowed.
#' @return A populated `knowledge` object.
#'
#' @importFrom stats setNames
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
      stop("tier() needs at least one two-sided formula.", call. = FALSE) # nocov
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
            stats::setNames(seq_along(kn$vars$var), kn$vars$var)
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
        kn <<- add_to_tier(kn, fml) # already exists, so just attach
        next
      }

      # create new tier after the current last one
      after_anchor <- if (nrow(kn$tiers)) utils::tail(kn$tiers$label, 1) else NULL

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

  exogenous <- function(...) {
    # capture the raw expressions the user typed
    specs <- rlang::enexprs(...)

    if (length(specs) == 0L) {
      stop("exogenous() needs at least one variable specification.", call. = FALSE)
    }

    # if they passed >1 selector, splice them into a single c(...) call
    vars_expr <- if (length(specs) == 1L) {
      specs[[1]]
    } else {
      rlang::expr(c(!!!specs))
    }

    # inject that call straight into add_exogenous()
    kn <<- add_exogenous(
      kn,
      !!vars_expr
    )
  }
  # synonyms for exogenous
  exo <- exogenous
  root <- exogenous

  # evaluate the call list
  allowed <- c("tier", "forbidden", "required", "exogenous", "exo", "root")
  for (expr in dots) {
    if (!is.call(expr) || !(as.character(expr[[1]]) %in% allowed)) {
      stop("Only tier(), forbidden(), required(), and exogenous()
           (as well as exo() and root() as synonyms) calls are allowed.",
        call. = FALSE
      )
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
#' @return The updated `knowledge` object.
#' @export
add_vars <- function(.kn, vars) {
  stopifnot(inherits(.kn, "knowledge"), is.character(vars))

  missing <- setdiff(vars, .kn$vars$var)

  if (.kn$frozen && length(missing)) {
    stop(
      "Unknown variable(s): [", paste(missing, collapse = ", "),
      "]\nThey are not present in the data frame provided to this knowledge object.",
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
#' @param .kn A knowledge object.
#' @param tier Bare symbol / character (label) **or** numeric literal.
#' @param before,after  Optional anchor relative to an existing tier label,
#'  tier index, or variable.  Once the knowledge object already
#'  has >= 1 tier, you must supply **exactly one** of these.
#'
#' @return The updated `knowledge` object.
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
#' @param .kn A `knowledge` object.
#' @param ...  One or more two-sided formulas `tier ~ vars`.
#'
#' @return The updated `knowledge` object.
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

#' Add forbidden edges
#'
#' @description
#' Forbid one or more directed edges.
#' Each argument **must** be a two–sided formula, e.g. `X ~ Y`.
#' Formulas can use tidy-select on either side, so
#' `forbid_edge(kn, starts_with("X") ~ Y)` forbids every `X_i --> Y`.
#'
#' @param .kn  A `knowledge` object.
#' @param ...  One or more two-sided formulas.
#'
#' @return The updated `knowledge` object.
#' @export
forbid_edge <- function(.kn, ...) {
  dots <- rlang::enquos(...)
  if (!length(dots)) {
    stop("forbid_edge() needs at least one two-sided formula.", call. = FALSE)
  }

  for (formula in dots) {
    .kn <- .edge_verb(.kn, "forbidden", formula)
  }
  .kn
}

#' Add required edges
#'
#' @description
#' Require one or more directed edges.
#' Arguments follow the same rules as **`forbid_edge()`** but a required edge
#' may only be given in *one* direction (`X ~ Y` **or** `Y ~ X`, not both).
#'
#' @inheritParams forbid_edge
#' @return The updated `knowledge` object.
#' @export
require_edge <- function(.kn, ...) {
  dots <- rlang::enquos(...)
  if (!length(dots)) {
    stop("require_edge() needs at least one two-sided formula.", call. = FALSE)
  }

  for (formula in dots) {
    .kn <- .edge_verb(.kn, "required", formula)
  }
  .kn
}

#' @title Add exogenous variables
#'
#' @description
#' Adds variables that cannot have incoming edges (root/exogenous nodes).
#' Every possible incoming edge to these nodes is automatically forbidden.
#' This is equivalent to writing `forbidden(everything() ~ vars)`.
#'
#' @param .kn A knowledge object.
#' @param vars Tidyselect specification or character vector of variables.
#'
#' @return Updated knowledge object.
#' @export
add_exogenous <- function(.kn, vars) {
  check_knowledge_obj(.kn)
  .kn <- forbid_edge(.kn, everything() ~ {{ vars }})
  .kn
}

#' @rdname add_exogenous
#' @export
add_exo <- add_exogenous

#' @rdname add_exogenous
#' @export
add_root <- add_exogenous

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
#'
#' @param x A `knowledge` object.
#' @param ... Additional arguments (not used).
#' @exportS3Method print knowledge
print.knowledge <- function(x, ...) {
  cli::cli_h1("Knowledge object")

  if (nrow(x$tiers)) {
    cat("\n")
    cli::cli_h2("Tiers")
    # print tibble without column and tibble info
    print_this <- format(tibble::as_tibble(x$tiers))[-1L][-2L]

    hdr <- print_this[1]
    pad <- sub("label.*", "", hdr)
    tail <- sub(".*label", "", hdr)
    print_this[1] <- paste0(
      pad,
      "\u001b[1mlabel\u001b[22m",
      tail
    )

    cat(print_this, sep = "\n")
  }

  if (nrow(x$vars)) {
    cat("\n")
    cli::cli_h2("Variables")
    # print tibble without column and tibble info
    print_this <- format(tibble::as_tibble(x$vars))[-1L][-2L]
    # format header
    hdr <- print_this[1]
    pad <- sub("var.*", "", hdr) # "  "
    mid <- sub(".*var(.*)tier.*", "\\1", hdr) # "   "
    tail <- sub(".*tier", "", hdr) # " "
    print_this[1] <- paste0(
      pad,
      "\u001b[1mvar\u001b[22m",
      mid,
      "\u001b[1mtier\u001b[22m",
      tail
    )
    cat(print_this, sep = "\n")
  }

  if (nrow(x$edges)) {
    cat("\n")
    cli::cli_h2("Edges")
    sym_arrow <- cli::symbol$arrow_right
    for (i in seq_len(nrow(x$edges))) {
      st <- x$edges$status[i]
      from <- x$edges$from[i]
      to <- x$edges$to[i]

      bullet <- switch(st,
        forbidden = cli::col_red(cli::symbol$cross),
        required  = cli::col_green(cli::symbol$tick),
        cli::symbol$bullet
      )

      cli::cat_line(
        " ", bullet, "  ", from, " ", sym_arrow, " ", to
      )
    }
  }
  cat("\n")
  invisible(x)
}

# ────────────────────────────── Manipulation ──────────────────────────────────
#' @title Merge two `knowledge` objects
#' @param .kn1 A `knowledge` object.
#' @param .kn2 Another `knowledge` object.
#' @exportS3Method "+" knowledge
`+.knowledge` <- function(.kn1, .kn2) {
  stopifnot(inherits(.kn1, "knowledge"), inherits(.kn2, "knowledge"))

  # combine
  vars_all <- unique(c(.kn1$vars$var, .kn2$vars$var))
  out <- .new_knowledge(vars_all)

  # var tiers
  vtiers <- dplyr::bind_rows(.kn1$vars, .kn2$vars) |>
    dplyr::group_by(var) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  # merge vars
  out$vars$tier <- vtiers$tier[match(out$vars$var, vtiers$var)]

  # merge tier labels, preserving .kn1 order then any new from .kn2
  all_labels <- unique(c(.kn1$tiers$label, .kn2$tiers$label))
  out$tiers <- tibble::tibble(label = all_labels)

  # merge edges (status, from, to, tier_from, tier_to are all character)
  out$edges <- dplyr::distinct(dplyr::bind_rows(.kn1$edges, .kn2$edges)) |>
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
#' @param .kn A `knowledge` object.
#' @param order A vector that lists *every* tier exactly once, either by
#'  label (default) or by numeric index (`by_index = TRUE`).
#'  Be careful if you have numeric tier labels.
#' @param by_index If `TRUE`, treat `order` as the positions instead of
#'  labels. Defaults to `FALSE`.
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
      # nocov start
      # this is a fallback that currently can't be reached, but will be kept
      # there for future-proofing.
      if (is.character(val) && nzchar(val)) {
        return(val)
      }
      # nocov end
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
#' @param tier The tier to move (label or index, honouring `by_index`).
#' @param before Exactly one of these must be supplied and must identify
#'  another existing tier.
#' @param after Exactly one of these must be supplied and must identify
#'  another existing tier.
#'
#' @return The updated `knowledge` object.
#' @export
reposition_tier <- function(.kn,
                            tier,
                            before = NULL,
                            after = NULL,
                            by_index = FALSE) {
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

# ───────────────────────────────── Remove ─────────────────────────────────────
#' @title Remove variables (and their edges) from a knowledge object
#'
#' @description
#' Drops the given variables from `kn$vars`, and automatically removes
#' any edges that mention them.
#'
#' @param .kn   A `knowledge` object.
#' @param ...   Unquoted variable names or tidy‐select helpers.
#'
#' @return An updated `knowledge` object.
#'
#' @importFrom purrr map
#' @export
remove_vars <- function(.kn, ...) {
  check_knowledge_obj(.kn)
  specs <- rlang::enquos(..., .ignore_empty = "all")

  # resolve each quosure to a character vector of names
  vars_list <- purrr::map(specs, function(q) {
    .vars_from_spec(.kn, rlang::get_expr(q))
  })
  vars <- unique(unlist(vars_list, use.names = FALSE))

  if (length(vars) == 0L) {
    stop("remove_vars() matched no variables.", call. = FALSE)
  }

  # drop them from the var table
  .kn$vars <- dplyr::filter(.kn$vars, !var %in% vars)

  # drop any edges that mention them
  .kn$edges <- dplyr::filter(
    .kn$edges,
    !from %in% vars,
    !to %in% vars
  )

  .kn
}

#' @title Remove edges from a knowledge object
#' @description
#' Drop any directed edge(s) matching the two‐sided formulas you supply.
#' Errors if no edges matched.
#' @param .kn   A `knowledge` object.
#' @param ...   One or more two‐sided formulas, e.g. `A ~ B` or `starts_with("X") ~ Y`.
#' @return The updated `knowledge` object.
#' @importFrom purrr map_dfr
#' @export
remove_edges <- function(.kn, ...) {
  check_knowledge_obj(.kn)
  specs <- rlang::enquos(..., .ignore_empty = "all")
  if (length(specs) == 0L) {
    stop("remove_edges() needs at least one two-sided formula.", call. = FALSE)
  }

  # build a little tibble of all (from,to) pairs the user wants to drop
  drop_tbl <- purrr::map_dfr(specs, function(fq) {
    expr <- rlang::get_expr(fq)
    from_ <- .formula_vars(.kn, rlang::f_lhs(expr))
    to_ <- .formula_vars(.kn, rlang::f_rhs(expr))
    tidyr::crossing(from = from_, to = to_)
  })

  # did any of those actually exist in kn$edges?
  matched <- dplyr::inner_join(
    drop_tbl,
    dplyr::select(.kn$edges, from, to),
    by = c("from", "to")
  )
  if (nrow(matched) == 0L) {
    stop("remove_edges() matched no edges.", call. = FALSE)
  }

  # drop them
  .kn$edges <- dplyr::anti_join(.kn$edges, drop_tbl, by = c("from", "to"))
  .kn
}

#' @title Remove entire tiers from a knowledge object
#'
#' @description
#' Drops tier definitions (and un‐tiers any vars assigned to them).
#'
#' @param .kn   A `knowledge` object.
#' @param ...   Tier labels (unquoted or character) or numeric indices.
#'
#' @return An updated `knowledge` object.
#' @importFrom purrr map_chr
#' @export
remove_tiers <- function(.kn, ...) {
  check_knowledge_obj(.kn)
  specs <- rlang::enquos(..., .ignore_empty = "all")
  keep <- .kn$tiers$label
  to_drop <- purrr::map_chr(specs, function(q) {
    val <- rlang::eval_tidy(q, .kn$tiers, env = parent.frame())
    if (is.numeric(val)) {
      return(.kn$tiers$label[val])
    }
    as.character(val)
  })

  to_drop <- intersect(to_drop, keep)
  if (!length(to_drop)) {
    return(.kn)
  }

  # drop the tier rows
  .kn$tiers <- dplyr::filter(.kn$tiers, !label %in% to_drop)

  # reset any vars that were in those tiers
  .kn$vars$tier[.kn$vars$tier %in% to_drop] <- NA_character_

  .kn
}

# ───────────────────────────────── Deparse ────────────────────────────────────
#' @title Deparse a knowledge object to knowledge() mini-DSL code
#'
#' @description
#' Given a `knowledge` object, return a single string containing
#' the R code (using `knowledge()`, `tier()`, `forbidden()`, `required()`)
#' that would rebuild that same object.
#'
#' @param .kn A `knowledge` object.
#' @param df_name Optional name of the data frame you used
#' (used as the first argument to `knowledge()`).  If `NULL`,
#' `knowledge()` is called with no data frame.
#'
#' @return A single string (with newlines) of R code.
#' @export
deparse_knowledge <- function(.kn, df_name = NULL) {
  check_knowledge_obj(.kn)

  fmt_fml <- function(lhs, rhs_vars) {
    paste0(
      as.character(lhs),
      " ~ ",
      paste(as.character(rhs_vars), collapse = " + ")
    )
  }

  out <- character()

  # ---- header ----
  if (is.null(df_name)) {
    out <- c(out, "knowledge(")
  } else {
    out <- c(out, paste0("knowledge(", df_name, ","))
  }

  # ---- tiers ----
  if (nrow(.kn$tiers)) {
    tier_labels <- .kn$tiers$label
    tier_fmls <- vapply(
      tier_labels,
      function(lbl) {
        vars <- .kn$vars$var[.kn$vars$tier == lbl]
        fmt_fml(lbl, vars)
      },
      character(1)
    )

    out <- c(
      out,
      "  tier(",
      paste0("    ", tier_fmls, collapse = ",\n"),
      "  ),"
    )
  }

  # ---- forbidden edges ----
  forb <- dplyr::filter(.kn$edges, status == "forbidden")
  if (nrow(forb)) {
    # group by 'from'
    f_grouped <- split(forb$to, forb$from)
    forb_fmls <- vapply(
      names(f_grouped),
      function(fm) fmt_fml(fm, f_grouped[[fm]]),
      character(1)
    )

    out <- c(
      out,
      "  forbidden(",
      paste0("    ", forb_fmls, collapse = ",\n"),
      "  ),"
    )
  }

  # ---- required edges ----
  req <- dplyr::filter(.kn$edges, status == "required")
  if (nrow(req)) {
    r_grouped <- split(req$to, req$from)
    req_fmls <- vapply(
      names(r_grouped),
      function(fm) fmt_fml(fm, r_grouped[[fm]]),
      character(1)
    )

    out <- c(
      out,
      "  required(",
      paste0("    ", req_fmls, collapse = ",\n"),
      "  ),"
    )
  }

  # ---- footer ----
  # drop trailing comma on the last line
  last <- length(out)
  out[last] <- sub(",$", "", out[last])
  out <- c(out, ")")

  # combine
  paste(out, collapse = "\n")
}

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────── Conversion to Tetrad, pcalg, bnlearn  ─────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Convert to Tetrad `edu.cmu.tetrad.data.Knowledge`
#'
#' @description
#' Converts a `knowledge` object to a Tetrad `edu.cmu.tetrad.data.Knowledge`.
#' This requires `rJava`. This is used internally, when setting knowledge with
#' `set_knowledge` for methods using the Tetrad engine. `set_knowledge` is used
#' internally, when using the `disco` function with knowledge given.
#' @param .kn A `knowledge` object.
#' @importFrom rJava .jinit .jnew
#' @importFrom purrr pwalk
#'
#' @export
as_tetrad_knowledge <- function(.kn) {
  check_knowledge_obj(.kn)
  if (!requireNamespace("rJava", quietly = TRUE)) { # nocov start
    stop("Package 'rJava' is required for as_tetrad_knowledge().")
  }
  if (!.jniInitialized) {
    .jinit(
      # todo: how many gb?
      parameters = "-Xmx2g",
      classpath = "inst/java/tetrad-current.jar"
    )
  } # nocov end

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

#' Convert background knowledge to pcalg constraint matrices
#'
#' pcalg only supports _undirected_ (symmetric) background constraints:
#' * **fixedGaps**  — forbidding edges (zeros enforced)
#' * **fixedEdges** — requiring edges (ones enforced)
#'
#' This function takes a \code{knowledge} object (with only forbidden/required
#' edges, no tiers) and returns the two logical matrices in the exact
#' variable order you supply.
#'
#' @param .kn A \code{knowledge} object.  Must have no tier information.
#' @param labels Character vector of all variable names, in the exact order
#'   of your data columns.  Every variable referenced by an edge in \code{.kn}
#'   must appear here.
#' @param directed_as_undirected Logical (default \code{FALSE}).  If
#'   \code{FALSE}, we require that every edge in \code{.kn} has its
#'   mirror-image present as well, and will error if any are missing.  If
#'   \code{TRUE}, we automatically mirror every directed edge into
#'   an undirected constraint.
#'
#' @return A list with two elements, each an \code{n × n} logical matrix
#' corresponding to `pcalg`'s `fixedGaps` and `fixedEdges` arguments.
#'
#' @section Errors:
#' * If the knowledge object contains tiered knowledge.
#' * If \code{directed_as_undirected = FALSE} and any edge lacks its
#'   symmetrical counterpart. This can only hold for forbidden edges.
#'
#' @export
as_pcalg_constraints <- function(.kn,
                                 labels = .kn$vars$var,
                                 directed_as_undirected = FALSE) {
  check_knowledge_obj(.kn)

  if (any(!is.na(.kn$vars$tier))) {
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
  if (!setequal(labels, .kn$vars$var)) {
    # all labels that aren't in knowledge
    bad_vars <- setdiff(labels, .kn$vars$var)
    if (length(bad_vars)) {
      stop("`labels` contained variables that were not in the knowledge object: [",
        paste(bad_vars, collapse = ", "), "]",
        call. = FALSE
      )
    }
    # all vars that aren't in labels
    missing_vars <- setdiff(.kn$vars$var, labels)
    if (length(missing_vars)) {
      stop("`labels` must contain all variables in the knowledge object: [",
        paste(missing_vars, collapse = ", "),
        "]\nYou can add variables to your knowledge object with add_vars().",
        call. = FALSE
      )
    } else {
      # nocov start
      # this is a future-proofing security measure, not reachable as of now
      stop("`labels` must contain all variables in the knowledge object.",
        call. = FALSE
      )
    }
    # nocov end
  }

  p <- length(labels)
  fixedGaps <- matrix(FALSE, p, p, dimnames = list(labels, labels))
  fixedEdges <- matrix(FALSE, p, p, dimnames = list(labels, labels))
  idx <- rlang::set_names(seq_along(labels), labels)

  if (!directed_as_undirected) {
    bad <- .kn$edges |>
      dplyr::anti_join(.kn$edges,
        by = c("from" = "to", "to" = "from")
      ) |>
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
  forb <- dplyr::filter(.kn$edges, status == "forbidden")
  for (k in seq_len(nrow(forb))) {
    i <- match(forb$from[k], labels, nomatch = NA_integer_)
    j <- match(forb$to[k], labels, nomatch = NA_integer_)
    # extra security measure
    if (is.na(i) || is.na(j)) {
      stop("Forbidden edge refers to unknown variable(s).", call. = FALSE)
    }
    fixedGaps[i, j] <- TRUE
    if (directed_as_undirected) fixedGaps[j, i] <- TRUE
  }

  # fill required
  req <- dplyr::filter(.kn$edges, status == "required")
  for (k in seq_len(nrow(req))) {
    i <- match(req$from[k], labels, nomatch = NA_integer_)
    j <- match(req$to[k], labels, nomatch = NA_integer_)
    # extra security measure
    if (is.na(i) || is.na(j)) {
      stop("Forbidden edge refers to unknown variable(s).", call. = FALSE)
    }
    fixedEdges[i, j] <- TRUE
    if (directed_as_undirected) fixedEdges[j, i] <- TRUE
  }

  list(fixedGaps = fixedGaps, fixedEdges = fixedEdges)
}

#' Convert background knowledge to bnlearns white- and blacklists
#'
#' @description
#' Converts a `knowledge` object to a list of two data frames, namely
#' `whitelist` and `blacklist`, which can be used as arguments for
#' `bnlearn` algorithms. The `whitelist` contains all required edges, and the
#' `blacklist` containts all forbidden edges. Tiers will be made into forbidden
#' edges before running the conversion.
#'
#' @param .kn A \code{knowledge} object.  Must have no tier information.
#'
#' @return A list with two elements, `whitelist` and `blacklist`, each a data
#' frame containing the edges in a `from`, `to` format.
#'
#' @export
as_bnlearn_knowledge <- function(.kn) {
  check_knowledge_obj(.kn)

  # whitelist holds all required edges in a "from", "to" dataframe
  whitelist <- dplyr::filter(.kn$edges, status == "required") |>
    dplyr::select(from, to) |>
    as.data.frame()

  # blacklist holds all forbidden edges (including tier violations)
  blacklist <- forbid_tier_violations(.kn)$edges |>
    dplyr::filter(status == "forbidden") |>
    dplyr::select(from, to) |>
    as.data.frame()

  return(list(
    whitelist = whitelist,
    blacklist = blacklist
  ))
}

#' @title Forbid all tier violations
#'
#' @description
#' Given a `knowledge` object with variables already assigned to tiers,
#' forbids every directed edge that runs from a higher-numbered tier down
#' into a lower-numbered tier.
#' @param .kn A `knowledge` object.
#' @return The same `knowledge` object with new forbidden edges added.
#' @export
forbid_tier_violations <- function(.kn) {
  check_knowledge_obj(.kn)

  # build a named vector of tier rank
  tier_ranks <- set_names(
    seq_along(.kn$tiers$label),
    .kn$tiers$label
  )

  # annotate each var with its numeric rank
  vars <- .kn$vars |>
    dplyr::mutate(rank = tier_ranks[tier])

  # select & rename for "from" vs "to"
  vf <- vars |> dplyr::select(var_from = var, rank_from = rank)
  vt <- vars |> dplyr::select(var_to = var, rank_to = rank)

  # true cartesian crossing of those two tibbles
  bad <- tidyr::crossing(vf, vt) |>
    dplyr::filter(rank_from > rank_to)

  # add all those forbidden edges, dropping self-loops
  if (nrow(bad)) {
    new_edges <- dplyr::tibble(
      status    = "forbidden",
      from      = bad$var_from,
      to        = bad$var_to,
      tier_from = .kn$vars$tier[match(bad$var_from, .kn$vars$var)],
      tier_to   = .kn$vars$tier[match(bad$var_to, .kn$vars$var)]
    )

    # bind to existing, drop duplicates
    .kn$edges <- dplyr::distinct(
      dplyr::bind_rows(.kn$edges, new_edges)
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
#' \dontrun{
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
#' }
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

# ───────────────────────────── Edge helpers  ──────────────────────────────────
#' @title Add one or many edges to a knowledge object
#'
#' @param .kn A `knowledge` object.
#' @param status A string, either "forbidden" or "required".
#' @param from A tidyselect specification or character vector of variable names.
#' @param to A tidyselect specification or character vector of variable names.
#' @param remove_self_loops Logical. If `TRUE`, self-loops are removed.
#' Defaults to `FALSE`.
#' @keywords internal
.add_edges <- function(.kn, status, from, to, remove_self_loops = TRUE) {
  # resolve `from` / `to` specs into character vectors of variable names
  from_chr <- .vars_from_spec(.kn, {{ from }})
  to_chr <- .vars_from_spec(.kn, {{ to }})

  # ensure all endpoint variables exist in `.kn$vars`
  .kn <- add_vars(.kn, unique(c(from_chr, to_chr)))

  # cartesian product
  # one row per directed edge, then annotate
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

  # merge into edge table, dropping duplicates, and return updated object
  .kn$edges <- dplyr::distinct(dplyr::bind_rows(.kn$edges, block))

  if (remove_self_loops) {
    .kn$edges <- dplyr::filter(.kn$edges, from != to)
  }

  # validate again for safety
  .validate_forbidden_required(.kn$edges)
  .kn
}

#' @title Handle forbid_edge() / require_edge() calls
#'
#' @description
#' Internal helper that turns each **two-sided formula** supplied by
#' `forbid_edge()` or `require_edge()` into concrete variable names, then passes
#' the cross-product to `.add_edges()`.
#'
#' @param .kn A `knowledge` object.
#' @param status Either `"forbidden"` or `"required"`.
#' @param fml A quosure that must wrap a two-sided formula.
#'
#' @keywords internal
.edge_verb <- function(.kn, status, fml) {
  if (!rlang::quo_is_call(fml, "~")) {
    stop(
      "Edge specification must be a two-sided formula like `A ~ B`.",
      call. = FALSE
    )
  }

  expr <- rlang::get_expr(fml)

  from_vars <- .formula_vars(.kn, rlang::f_lhs(expr))
  to_vars <- .formula_vars(.kn, rlang::f_rhs(expr))

  if (!length(from_vars) || !length(to_vars)) {
    stop(sprintf("Formula `%s` matched no variables.", deparse(expr)),
      call. = FALSE
    )
  }
  .kn <- .add_edges(.kn, status, from_vars, to_vars)
  .kn
}

# ───────────────────────────── Misc helpers  ──────────────────────────────────
#' @title Resolve a tidy-select or character spec to character names
#'
#' @param .kn A `knowledge` object.
#' @param spec A tidyselect specification (e.g. `everything()`,
#' `starts_with("V")`) or a character vector.
#' @keywords internal
#' @title Resolve a tidy-select or character spec to character names
#'
#' @param .kn A `knowledge` object.
#' @param spec A tidyselect specification (e.g. `everything()`,
#'             `starts_with("V")`), a bare symbol, a character vector,
#'             *or* a literal c(V1, V2, "V3") call.
#' @keywords internal
#' @importFrom purrr map_chr
.vars_from_spec <- function(.kn, spec) {
  #  scalars can't be variable names
  if (is.atomic(spec) && length(spec) == 1L && !is.character(spec)) {
    return(character(0))
  }

  # literal c(...) of names --> turn into a plain character vector
  if (rlang::is_call(spec, "c")) {
    args <- as.list(spec)[-1]
    out <- purrr::map_chr(args, function(arg) {
      # if they wrote c(V4), arg is a symbol
      if (rlang::is_symbol(arg)) {
        return(rlang::as_string(arg))
      }
      # if they wrote c("V4"), arg is a literal string
      if (is.character(arg) && length(arg) == 1L) {
        return(arg)
      }
      stop(
        "Unsupported argument in c(): ",
        deparse(arg),
        call. = FALSE
      )
    })
    return(out)
  }

  # bare symbol --> maybe a user-supplied character, or a var name
  if (rlang::is_symbol(spec)) {
    out <- tryCatch(
      rlang::eval_tidy(spec, env = parent.frame()),
      error = function(e) NULL
    )
    if (is.character(out)) {
      return(out)
    }
    nm <- rlang::as_string(spec)
    if (nm %in% .kn$vars$var) {
      return(nm)
    }
    return(character(0))
  }

  # character vector --> wrap in all_of()
  if (is.character(spec)) {
    q <- rlang::quo(dplyr::all_of(spec))
  } else {
    # 5) otherwise, a tidy-select expression → leave it as a quosure
    q <- rlang::as_quosure(spec, env = parent.frame())
  }

  # fall back to tidyselect
  vars <- tryCatch(
    names(tidyselect::eval_select(q, set_names(seq_along(.kn$vars$var), .kn$vars$var))),
    error = function(e) character(0)
  )
  if (length(vars)) {
    return(vars)
  }

  # final fallback: evaluate as plain R
  out2 <- tryCatch(
    rlang::eval_tidy(spec, env = parent.frame()),
    error = function(e) NULL
  )
  if (is.character(out2)) {
    return(out2)
  }

  # nothing matched
  character(0)
}


#' @title Extract variable names from the RHS of a `tier()` formula
#'
#' @param .kn A `knowledge` object.
#' @param rhs A formula (e.g. `1 ~ V1 + V2`).
#' @keywords internal
.formula_vars <- function(.kn, rhs) {
  vars <- .vars_from_spec(.kn, rhs)
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
#' @importFrom cli cli_h1 cli_h2 symbol
NULL
