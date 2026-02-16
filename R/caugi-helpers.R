#' @title Retrieve Nodes
#'
#' @description
#' This function retrieves the nodes from a `caugi` object as a tibble.
#'
#' @param cg A `caugi` object.
#' @return A tibble containing the nodes.
#'
#' @examples
#' data(tpc_example)
#' cd_tges <- tges(engine = "causalDisco", score = "tbic")
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#' nodes(disco_cd_tges$caugi)
#' @keywords internal
#' @noRd
nodes <- function(cg) {
  tibble::as_tibble(caugi::nodes(cg))
}

#' @title Retrieve Edges
#'
#' @description
#' This function retrieves the edges from a `caugi` object as a tibble.
#'
#' @param cg A `caugi` object.
#' @return A tibble containing the edges.
#'
#' @examples
#' data(tpc_example)
#' cd_tges <- tges(engine = "causalDisco", score = "tbic")
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#' edges(disco_cd_tges$caugi)
#' @keywords internal
#' @noRd
edges <- function(cg) {
  tibble::as_tibble(caugi::edges(cg))
}
