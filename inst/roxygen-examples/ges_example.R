### ges() example ###
\dontrun{
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
my_ges <- ges(engine = "tetrad", score = "sem_bic")

disco(tpcExample, my_ges, knowledge = kn)

# or using my_ges directly
my_ges <- my_ges |> set_knowledge(kn)
my_ges(tpcExample)
}
