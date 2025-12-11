### .build_knowledge_from_order() example ###

data(tpc_example)
vnames <- names(tpc_example)

# Create knowledge from a given order
kn_from_order <- causalDisco:::.build_knowledge_from_order(
  order  = c("child", "youth", "oldage"),
  vnames = vnames
)

kn_from_order
