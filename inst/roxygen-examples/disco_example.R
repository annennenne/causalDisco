### disco() example ###

data("tpcExample")

# define background knowledge
kn <- knowledge(
  tpcExample,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# use PC (for example)
my_pc <- pc(engine = "tetrad", test = "fisher_z", alpha = 0.01)

disco(data = tpcExample, method = my_pc)
