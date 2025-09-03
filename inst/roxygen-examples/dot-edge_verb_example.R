# add a required edge via the internal edge verb using a quosure
data(tpcExample)
kn <- knowledge(tpcExample)

# create quosure using global environment
q <- rlang::quo(child_x1 ~ youth_x3)

# create edge using the internal function
kn <- causalDisco:::.edge_verb(kn, "required", q)

print(kn)

# error path
try(
  causalDisco:::.edge_verb(
    kn, "forbidden",
    rlang::quo(child_x1 ~ non_existent_var)
  )
)
try(
  causalDisco:::.edge_verb(
    kn, "invalid_type",
    rlang::quo(child_x1 ~ youth_x3)
  )
)
