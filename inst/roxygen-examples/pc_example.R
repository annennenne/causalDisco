### pc() example ###
\dontrun{
data("tpc_example")

kn <- knowledge(
  tpc_example,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    oldage ~ tidyselect::starts_with("oldage")
  )
)

# Recommended path using disco()
my_pc <- pc(engine = "tetrad", test = "fisher_z", alpha = 0.05)

disco(tpc_example, my_pc, knowledge = kn)

# or using my_pc directly
my_pc <- my_pc |> set_knowledge(kn)
my_pc(tpc_example)
}
