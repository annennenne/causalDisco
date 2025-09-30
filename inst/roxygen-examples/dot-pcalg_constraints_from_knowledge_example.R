### .pcalg_constraints_from_knowledge() example ###
data(tpcExample)

kn <- knowledge(
  tpcExample,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

pc_cons <- causalDisco:::.pcalg_constraints_from_knowledge(
  kn,
  labels = names(tpcExample),
  directed_as_undirected = TRUE
)
pc_cons
