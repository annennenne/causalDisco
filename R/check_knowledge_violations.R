# This function should probably also check edge type.
#' Check for tier violations in a causal graph
#' This function checks whether any edges in the provided edge list violate the tier
#' knowledge constraints defined in the knowledge object.
#'
#' @param edges A data.table containing the edges of the causal graph with columns 'from' and 'to'.
#' @param kn A knowledge() object containing tier information in kn$vars (with columns 'var' and 'tier').
#' @return A tibble of edges that violate the tier constraints. If no violations are found, an empty tibble is returned.
#' @keywords internal
#' @noRd
check_tier_violations <- function(edges, kn) {
  tier_map <- kn$vars # tibble: var | tier

  # Make tier ordered by appearance
  tier_map <- kn$vars |>
    dplyr::mutate(
      tier = factor(tier, levels = unique(tier), ordered = TRUE),
      tier_num = as.integer(tier)
    ) |>
    dplyr::select(var, tier_num)

  edges_with_tiers <- edges |>
    dplyr::as_tibble() |>
    dplyr::left_join(tier_map, by = c("from" = "var")) |>
    dplyr::rename(tier_from = tier_num) |>
    dplyr::left_join(tier_map, by = c("to" = "var")) |>
    dplyr::rename(tier_to = tier_num)

  violations <- edges_with_tiers |>
    dplyr::filter(
      edge != "<->", # bidirectional edges are allowed between tiers (means latent confounder)
      tier_from > tier_to
    )

  violations
}

#' @title Check for edge constraint violations in a causal graph
#' @description
#' This function checks whether the provided edges violate any required or forbidden
#' edge constraints defined in the knowledge object.
#'
#' @param edges A data.table containing the edges of the causal graph with columns 'from' and 'to'.
#' @param kn A knowledge() object containing edge constraints in kn$edges (with columns 'from', 'to', and 'status').
#' @return A tibble of edges that violate the constraints. If no violations are found, an empty tibble is returned.
#' @keywords internal
#' @noRd
check_edge_constraints <- function(edges, kn) {
  # User-provided edges
  edges_tbl <- edges |> dplyr::as_tibble()

  # All constraints from knowledge()
  constraints <- kn$edges |> dplyr::as_tibble()

  # ---- REQUIRED CHECKS ----
  required_edges <- constraints |> dplyr::filter(status == "required")

  required_violations <- required_edges |>
    dplyr::anti_join(edges_tbl, by = c("from", "to")) |>
    dplyr::mutate(violation_type = "missing_required")

  # ---- FORBIDDEN CHECKS ----
  forbidden_edges <- constraints |> dplyr::filter(status == "forbidden")

  forbidden_violations <- forbidden_edges |>
    dplyr::semi_join(edges_tbl, by = c("from", "to")) |>
    dplyr::mutate(violation_type = "present_forbidden")

  # Combine all violations
  dplyr::bind_rows(required_violations, forbidden_violations)
}
