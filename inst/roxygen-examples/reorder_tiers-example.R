# Move one tier relative to another
data(tpc_example)

kn <- knowledge(
  head(tpc_example),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  )
)

kn <- reorder_tiers(kn, c("youth", "child", "oldage"))
print(kn)
