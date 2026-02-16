.engines <- c("bnlearn", "causalDisco", "pcalg", "tetrad")

#' Supported Engines
#'
#' Available engines: `r paste(.engines, collapse = ", ")`.
#'
#' @keywords internal
#' @noRd
engine_registry <- as.list(.engines)
