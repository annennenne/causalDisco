### reposition_tier_example.R ###

# move one tier relative to another
data(tpcExample)

kn <- knowledge(
  head(tpcExample),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  )
)

kn <- reorder_tiers(kn, c("youth", "child", "oldage"))
print(kn)
