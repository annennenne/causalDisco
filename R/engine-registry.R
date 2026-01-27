.engines <- c("bnlearn", "causalDisco", "pcalg", "tetrad")

#' Supported Engines
#'
#' Available engines: `r paste(.engines, collapse = ", ")`.
#'
#' @export
engine_registry <- as.list(.engines)
