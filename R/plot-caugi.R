#' Plot a Causal Graph from a `knowledgeable_caugi` Object
#'
#' This function visualizes a causal graph stored within a `knowledgeable_caugi` object.
#' It extends [plot.knowledge()] by combining the causal graph from a `caugi` object with
#' background knowledge, highlighting required and forbidden edges.
#'
#' - **Required edges** are drawn in **blue**.
#' - **Forbidden edges** are drawn in **red**.
#' - If tiered knowledge is provided, nodes are arranged according to their tiers.
#'
#' @inheritParams plot.knowledge
#' @param x A `knowledgeable_caugi` object containing both the causal graph and the associated knowledge.
#' @param ... Additional arguments passed to [caugi::plot()] and [plot.knowledge()].
#'
#' @return Invisibly returns the underlying `caugi` object. The main effect is the plot.
#'
#' @details
#' This function combines the causal graph and the knowledge object into a single plotting
#' structure. If the knowledge contains tiers, nodes are laid out accordingly. Otherwise, the
#' default igraph layout is used. Edge styling is automatically applied based on the knowledge:
#' required edges are blue, forbidden edges are red and dashed.
#'
#' @examples
#' data(tpc_example)
#'
#' # Define tiered knowledge
#' kn <- knowledge(
#'   tpc_example,
#'   tier(
#'     child ~ starts_with("child"),
#'     youth ~ starts_with("youth"),
#'     old ~ starts_with("old")
#'   )
#' )
#'
#' # Fit a causal discovery model
#' cd_tges <- tges(engine = "causalDisco", score = "tbic")
#' disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)
#'
#' # Plot with default column orientation
#' plot(disco_cd_tges)
#'
#' # Plot with row orientation
#' plot(disco_cd_tges, orientation = "rows")
#'
#' # Plot without tiers
#' kn_untiered <- knowledge(
#'   tpc_example,
#'   child_x1 %-->% c(child_x2, youth_x3),
#'   youth_x4 %!-->% oldage_x5
#' )
#' cd_untiered <- disco(data = tpc_example, method = cd_tges, knowledge = kn_untiered)
#' plot(cd_untiered)
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
#'
#' - **Required edges** are drawn in **blue**.
#' - **Forbidden edges** are drawn in **red**.
#' - If tiered knowledge is provided, nodes are arranged according to their tiers.
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
