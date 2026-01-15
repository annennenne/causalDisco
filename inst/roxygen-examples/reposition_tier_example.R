### reposition_tier_example.R ###

# move one tier relative to another
data(tpc_example)

kn <- knowledge(
  head(tpc_example),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  )
)

kn <- reposition_tier(kn, tier = oldage, before = "youth")
print(kn)
