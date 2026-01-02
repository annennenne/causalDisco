# success case: consistent declarations
edges_ok <- tibble::tibble(
  status = c("required", "forbidden"),
  from = c("child_x1", "oldage_x6"),
  to = c("youth_x3", "child_x1"),
  tier_from = c("child", "oldage"),
  tier_to = c("youth", "child")
)
causalDisco:::.validate_forbidden_required(edges_ok)

# error case: both forbidden and required for the same ordered pair
edges_bad <- tibble::tibble(
  status = c("required", "forbidden"),
  from = c("child_x1", "child_x1"),
  to = c("youth_x3", "youth_x3"),
  tier_from = c("child", "child"),
  tier_to = c("youth", "youth")
)
try(causalDisco:::.validate_forbidden_required(edges_bad))
