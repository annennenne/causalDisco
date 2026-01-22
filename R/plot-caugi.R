#' Plot a Causal Graph from a `knowledgeable_caugi` Object
#'
#' This function visualizes a causal graph stored within a `caugi` object.
#' It incorporates background knowledge to highlight required and forbidden edges. The required
#' edges are drawn in blue, while forbidden edges are shown in red and are dashed. If tiered
#' knowledge is provided, the nodes are arranged according to their tiers; otherwise, a circular
#' layout is used.
#'
#' @param x A `caugi` object containing the causal graph and knowledge.
#' @param ... Additional arguments passed to igraph `plot` and `plot.knowledge`.
#' @return A plot of the causal graph.
#' @method plot knowledgeable_caugi
#' @examples
#' data(tpc_example)
#'
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#'
#' cd_tges <- tges(engine = "causalDisco", score = "tbic")
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#'
#' plot(disco_cd_tges)
#'
#' @export
plot.knowledgeable_caugi <- function(
  x,
  orientation = c("columns", "rows"),
  ...
) {
  orientation <- match.arg(orientation)
  info_object <- combine_knowledge_and_caugi(x$caugi, x$knowledge)
  cg <- info_object$caugi
  tiers <- info_object$tiers

  has_tiers <- !all(is.na(tiers))

  if (has_tiers) {
    plot(cg, tiers = tiers, orientation = orientation, ...)
  } else {
    plot(cg, ...)
  }
}


#' Plot a Knowledge Object
#'
#' Visualize a `knowledge` object as a directed graph using [caugi::plot()].
#' If tiers are defined, nodes will be arranged according to their tier.
#'
#' @param x A `knowledge` object, created using [knowledge()].
#' @param orientation Character(1). Orientation of the tiers in the plot.
#'   Either `"columns"` (default) or `"rows"`. Only used if tiered knowledge is provided.
#' @param ... Additional arguments passed to [caugi::plot()], e.g., `node_color`, `edge_color`.
#'
#' @return Invisibly returns the caugi object used for plotting. The main effect is the plot.
#'
#' @details
#' - Nodes are arranged by tiers if tier information is provided in the knowledge object.
#' - If some nodes are missing tier assignments, a warning is issued and the plot falls back
#'   to untiered plotting.
#' - The function automatically handles edges marked as "required" or "forbidden" in the knowledge object.
#'
#' @examples
#' data(tpc_example)
#'
#' # Define a knowledge object with tiers
#' kn_tiered <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#'
#' # Simple plot (default column orientation)
#' plot(kn_tiered)
#'
#' # Plot with row orientation
#' plot(kn_tiered, orientation = "rows")
#'
#' # Plot with custom node/edge styling
#' # plot(kn_tiered, node_color = "lightblue", edge_color = "darkred")
#'
#' # Define a knowledge object without tiers
#' kn_untiered <- knowledge(
#'   tpc_example,
#'   child_x1 %-->% c(child_x2, youth_x3),
#'   youth_x4 %!-->% oldage_x5
#' )
#' plot(kn_untiered)
#'
#' @export
plot.knowledge <- function(x, orientation = c("columns", "rows"), ...) {
  orientation <- match.arg(orientation)
  info_object <- knowledge_to_caugi(x)
  cg <- info_object$caugi
  tiers <- info_object$tiers

  # Correct NA checks
  has_tiers <- length(tiers) > 0 &&
    !all(sapply(tiers, function(x) all(is.na(x))))
  any_na_tiers <- any(sapply(tiers, function(x) any(is.na(x))))

  if (has_tiers) {
    if (any_na_tiers) {
      warning(
        "Not all nodes are assigned to tiers. Tiered plotting not implemented for partial tiers. Defaulting to untiered plotting.",
        call. = FALSE
      )
      plot(cg, ...)
    } else {
      plot(
        cg,
        tiers = tiers,
        orientation = orientation,
        ...
      )
    }
  } else {
    plot(cg, ...)
  }
}
