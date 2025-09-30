### tfci() example ###

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
my_tfci <- tfci(engine = "causalDisco", corTest, alpha = 0.05)

disco(tpcExample, my_tfci, knowledge = kn)

# or using my_tfci directly
my_tfci <- my_tfci |> set_knowledge(kn)
my_tfci(tpcExample)

# Also possible: using tfci_run()
tfci_run(tpcExample, test = corTest, knowledge = kn)
