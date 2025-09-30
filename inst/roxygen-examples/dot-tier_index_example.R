### .tier_index() example ###

data(tpcExample)

kn <- knowledge(
  tpcExample,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

ti <- causalDisco:::.tier_index(kn, names(tpcExample)[1:6])
ti
