# This function should probably also check edge type.
#' Check for tier violations in a causal graph
#' This function checks whether any edges in the provided edge list violate the tier
#' knowledge constraints defined in the `Knowledge` object.
#'
#' @param edges A data.table containing the edges of the causal graph with columns 'from' and 'to'.
#' @param kn A knowledge() object containing tier information in kn$vars (with columns 'var' and 'tier').
#' @return A tibble of edges that violate the tier constraints. If no violations are found, an empty tibble is returned.
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
check_tier_violations <- function(edges, kn) {
  tier_map <- kn$vars # tibble: var | tier

  # Make tier ordered by appearance
  tier_map <- kn$vars |>
    dplyr::mutate(
      tier = factor(.data$tier, levels = unique(.data$tier), ordered = TRUE),
      tier_num = as.integer(.data$tier)
    ) |>
    dplyr::select("var", "tier_num")

  edges_with_tiers <- edges |>
    dplyr::as_tibble() |>
    dplyr::left_join(tier_map, by = c("from" = "var")) |>
    dplyr::rename(tier_from = "tier_num") |>
    dplyr::left_join(tier_map, by = c("to" = "var")) |>
    dplyr::rename(tier_to = "tier_num")

  violations <- edges_with_tiers |>
    dplyr::filter(
      !.data$edge %in% c("<->", "o-o"),
      .data$tier_from > .data$tier_to
    )

  violations
}


#' @title Check for edge constraint violations in a causal graph
#' @description
#' This function checks whether the provided edges violate any required or forbidden
#' edge constraints defined in the `Knowledge` object.
#'
#' @param edges A data.table containing the edges of the causal graph with columns 'from' and 'to'.
#' @param kn A knowledge() object containing edge constraints in kn$edges (with columns 'from', 'to', and 'status').
#' @return A tibble of edges that violate the constraints. If no violations are found, an empty tibble is returned.
#' @keywords internal
#' @noRd
#' @importFrom rlang .data
check_edge_constraints <- function(edges, kn) {
  edges_tbl <- edges |> dplyr::as_tibble()
  constraints <- kn$edges |> dplyr::as_tibble()

  # ---- REQUIRED CHECKS ----
  required_edges <- constraints |> dplyr::filter(.data$status == "required")

  required_allowed <- c("-->", "o->")

  required_violations <- required_edges |>
    dplyr::left_join(edges_tbl, by = c("from", "to")) |>
    dplyr::filter(is.na(.data$edge) | !(.data$edge %in% required_allowed)) |>
    dplyr::mutate(
      violation_type = "missing_required"
    ) |>
    dplyr::select("from", "to", "edge", "violation_type")

  # ---- FORBIDDEN CHECKS ----
  forbidden_edges <- constraints |> dplyr::filter(.data$status == "forbidden")

  forbidden_allowed <- c("<->", "<-o", "<--", "o-o")

  forbidden_violations <- forbidden_edges |>
    dplyr::inner_join(edges_tbl, by = c("from", "to")) |>
    dplyr::filter(!.data$edge %in% forbidden_allowed) |>
    dplyr::mutate(
      violation_type = "present_forbidden"
    ) |>
    dplyr::select("from", "to", "edge", "violation_type")

  # ---- COMBINE ----
  dplyr::bind_rows(required_violations, forbidden_violations)
}
