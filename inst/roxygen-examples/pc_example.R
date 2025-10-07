### pc() example ###

data("tpcExample")

kn <- knowledge(
  tpcExample,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

# Recommended path using disco()
my_pc <- pc(engine = "tetrad", test = "fisher_z", alpha = 0.05)

disco(tpcExample, my_pc, knowledge = kn)

# or using my_pc directly
my_pc <- my_pc |> set_knowledge(kn)
my_pc(tpcExample)
