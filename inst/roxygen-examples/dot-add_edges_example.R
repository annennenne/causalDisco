# add a block of forbidden edges using internal helper
data(tpcExample)

kn <- knowledge(tpcExample)

# add forbidden edges between youth_* and child_*
kn <- causalDisco:::.add_edges(kn, "forbidden",
  from = c("youth_x3", "youth_x4"),
  to = c("child_x1", "child_x2")
)

print(kn)
