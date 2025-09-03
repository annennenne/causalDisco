### remove_vars() example ###

# remove variables and their incident edges
data(tpcExample)

kn <- knowledge(
  head(tpcExample),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  ),
  required(child_x1 ~ youth_x3)
)
print(kn)

kn <- remove_edges(kn, child_x1 ~ youth_x3)
print(kn)

kn <- remove_vars(kn, starts_with("child_"))
print(kn)

kn <- remove_tiers(kn, "child")
print(kn)
