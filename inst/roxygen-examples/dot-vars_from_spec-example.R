# resolve different kinds of specs into character names
data(tpc_example)
kn <- knowledge(
  head(tpc_example)
)

# character vector
causalDisco:::.vars_from_spec(kn, c("child_x1", "youth_x3"))

# bare symbol that refers to a character vector
target <- c("oldage_x5", "oldage_x6")
causalDisco:::.vars_from_spec(kn, target)

# tidyselect expression
causalDisco:::.vars_from_spec(kn, rlang::expr(starts_with("child_")))
