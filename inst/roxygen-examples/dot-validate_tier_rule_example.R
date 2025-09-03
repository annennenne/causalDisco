### .validate_tier_rule() example ###

# success case: edge goes from child to youth (allowed)
tiers <- tibble::tibble(label = c("child", "youth"))
edges <- tibble::tibble(
  status    = "required",
  from      = "child_x1",
  to        = "youth_x3",
  tier_from = "child",
  tier_to   = "youth"
)
causalDisco:::.validate_tier_rule(edges, tiers)

# violation case: required edge from youth to child (should error)
edges_bad <- tibble::tibble(
  status    = "required",
  from      = "youth_x3",
  to        = "child_x1",
  tier_from = "youth",
  tier_to   = "child"
)
try(causalDisco:::.validate_tier_rule(edges_bad, tiers))
