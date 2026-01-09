# remove variables and their incident edges
data(tpc_example)

kn <- knowledge(
  head(tpc_example),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  ),
  child_x1 %-->% youth_x3
)
print(kn)

kn <- remove_edge(kn, child_x1, youth_x3)
print(kn)

kn <- remove_vars(kn, starts_with("child_"))
print(kn)

kn <- remove_tiers(kn, "child")
print(kn)
