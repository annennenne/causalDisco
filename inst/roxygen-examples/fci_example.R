### fci() example ###
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
my_fci <- fci(engine = "tetrad", test = "fisher_z", alpha = 0.05)

disco(tpc_example, my_fci, knowledge = kn)

# or using my_fci directly
my_fci <- my_fci |> set_knowledge(kn)
my_fci(tpc_example)
}
