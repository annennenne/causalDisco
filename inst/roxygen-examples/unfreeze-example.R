# unfreeze allows adding variables beyond the original data frame columns
data(tpc_example)

kn <- knowledge(tpc_example)

# this would error while frozen
try(add_vars(kn, "new_var"))

# unfreeze and add the new variable successfully
kn <- unfreeze(kn)
kn <- add_vars(kn, "new_var")

print(kn)
