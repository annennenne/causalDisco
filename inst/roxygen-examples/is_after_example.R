### is_after() example ###

data(tpcExample)

kn <- knowledge(
  tpcExample,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

causalDisco:::is_after("youth_x3", "child_x1", kn) # TRUE
causalDisco:::is_after("child_x2", "oldage_x6", kn) # FALSE
