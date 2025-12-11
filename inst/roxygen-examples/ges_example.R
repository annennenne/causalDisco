### ges() example ###
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
my_ges <- ges(engine = "tetrad", score = "sem_bic")

disco(tpc_example, my_ges, knowledge = kn)

# or using my_ges directly
my_ges <- my_ges |> set_knowledge(kn)
my_ges(tpc_example)
}
