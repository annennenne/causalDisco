# add a required edge via the internal edge verb using a quosure
data(tpc_example)
kn <- knowledge(tpc_example)

# create quosure using global environment
q <- rlang::quo(child_x1 ~ youth_x3)

# create edge using the internal function
kn <- .edge_verb(kn, "required", q)

print(kn)

# error path
try(
  .edge_verb(
    kn,
    "forbidden",
    rlang::quo(child_x1 ~ non_existent_var)
  )
)
try(
  .edge_verb(
    kn,
    "invalid_type",
    rlang::quo(child_x1 ~ youth_x3)
  )
)
