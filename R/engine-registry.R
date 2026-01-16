.engines <- c("bnlearn", "causalDisco", "pcalg", "tetrad")

#' Supported engines for causalDisco
#'
#' Available engines: `r paste(.engines, collapse = ", ")`.
#'
#' @export
engine_registry <- as.list(.engines)
