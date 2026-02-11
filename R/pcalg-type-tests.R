#' @title Resolve CI test and build sufficient statistic
#'
#' @description
#' Map a string identifier to the CI test function and compute the matching
#' sufficient statistic from `X`. Returns both.
#'
#' @param method Character; name of the CI test.
#' @param X data.frame, matrix, list of data.frames, or mice::mids.
#' @param suff_stat logical; if TRUE, compute sufficient statistic from `X`.
#' @param adaptDF logical for discrete tests
#' @param nlev optional integer vector of levels for discrete tests
#'
#' @returns `list(method = function, suff_stat = object)`
#' @noRd
#' @keywords internal
.get_pcalg_test_from_string <- function(
  method,
  X = NULL,
  suff_stat = FALSE,
  adaptDF = TRUE,
  nlev = NULL
) {
  method <- tolower(method)

  if (
    method %in%
      c(
        "fisher_z_twd",
        "fisher_z_mi",
        "conditional_gaussian",
        "conditional_gaussian_twd",
        "conditional_gaussian_mi",
        "g_square_twd",
        "g_square_mi"
      )
  ) {
    .check_if_pkgs_are_installed(
      pkgs = c("micd"),
      function_name = ".get_pcalg_test_from_string"
    )
  }

  g_square_switch <- function(x, y, S, suff_stat) {
    dm <- suff_stat$dm
    if (is.null(dm)) {
      stop("g_square requires suff_stat$dm.", call. = FALSE)
    }
    cols <- c(x, y, S)
    lev <- if (!is.null(suff_stat$nlev)) {
      suff_stat$nlev[cols]
    } else {
      vapply(cols, function(j) length(unique(dm[, j])), integer(1))
    }
    if (all(lev == 2L)) {
      pcalg::binCItest(x, y, S, suff_stat)
    } else {
      pcalg::disCItest(x, y, S, suff_stat)
    }
  }

  fun <- switch(
    method,
    "fisher_z" = pcalg::gaussCItest,
    "g_square" = g_square_switch,
    "reg" = reg_test,
    "fisher_z_twd" = micd::gaussCItwd,
    "fisher_z_mi" = micd::gaussMItest,
    "conditional_gaussian" = micd::mixCItest,
    "conditional_gaussian_twd" = micd::mixCItwd,
    "conditional_gaussian_mi" = micd::mixMItest,
    "g_square_twd" = micd::disCItwd,
    "g_square_mi" = micd::disMItest,
    stop(paste0("Unknown method: ", method), call. = FALSE)
  )

  if (suff_stat == FALSE) {
    return(list(method = fun, suff_stat = NULL))
  }
  if (is.null(X)) {
    stop("X must be provided to compute sufficient statistic.", call. = FALSE)
  }
  suff <- .get_suff_stat(
    X = X,
    method = method,
    adaptDF = adaptDF,
    nlev = nlev
  )

  list(method = fun, suff_stat = suff)
}

#' @title Build sufficient statistic for pcalg/micd/causalDisco tests
#' @keywords internal
#' @noRd
.get_suff_stat <- function(
  X,
  method,
  adaptDF = TRUE,
  nlev = NULL
) {
  method <- tolower(method)
  if (inherits(X, "matrix")) {
    X <- as.data.frame(X)
  }

  switch(
    method,

    # gaussian
    "fisher_z" = {
      X_num <- as.matrix(X)
      list(C = stats::cor(X_num), n = nrow(X_num))
    },
    "fisher_z_twd" = {
      as.matrix(X)
    },
    "fisher_z_mi" = {
      if (inherits(X, "mids")) {
        X <- mice::complete(X, action = "all")
      }
      if (!is.list(X)) {
        stop("gaussMItest requires a list or a mids object.", call. = FALSE)
      }
      C <- lapply(X, stats::cor)
      n <- nrow(X[[1]])
      c(C, n)
    },

    # discrete
    "g_square" = {
      out <- .to_dm_0_based(X, nlev = nlev)
      list(dm = out$dm, nlev = out$nlev, adaptDF = adaptDF)
    },
    "g_square_twd" = {
      out <- .to_dm_0_based(X, nlev = nlev)
      list(dm = out$dm, adaptDF = adaptDF)
    },
    "g_square_mi" = {
      if (inherits(X, "mids")) {
        X <- mice::complete(X, action = "all")
      }
      if (!is.list(X)) {
        stop("disMItest requires a list or a mids object.", call. = FALSE)
      }
      X
    },

    # mixed
    "conditional_gaussian" = {
      as.data.frame(X)
    },
    "conditional_gaussian_twd" = {
      as.data.frame(X)
    },
    "conditional_gaussian_mi" = {
      if (inherits(X, "mids")) {
        X <- mice::complete(X, action = "all")
      }
      if (!is.list(X)) {
        stop("mixMItest requires a list or a mids object.", call. = FALSE)
      }
      X
    },

    # regression test
    "reg" = {
      df <- as.data.frame(X)
      binary <- vapply(df, .classify_binary, logical(1))
      list(data = df, binary = binary)
    },
    stop("Unknown method: ", method, call. = FALSE)
  )
}

.to_dm_0_based <- function(X, nlev = NULL) {
  D <- as.data.frame(X)

  for (j in seq_along(D)) {
    if (!is.factor(D[[j]])) D[[j]] <- as.factor(D[[j]])
  }

  lev <- vapply(D, nlevels, integer(1))

  for (j in seq_along(D)) {
    D[[j]] <- as.integer(D[[j]]) - 1L
    if (min(D[[j]], na.rm = TRUE) != 0L) {
      D[[j]] <- D[[j]] - min(D[[j]], na.rm = TRUE)
    }
  }

  list(dm = as.matrix(D), nlev = if (is.null(nlev)) lev else nlev)
}

.classify_binary <- function(x) {
  if (is.logical(x)) {
    return(TRUE)
  }
  if (is.factor(x)) {
    return(nlevels(x) == 2L)
  }
  if (is.numeric(x)) {
    u <- sort(unique(stats::na.omit(x)))
    return(length(u) == 2L && all(u %in% c(0, 1)))
  }
  FALSE
}
