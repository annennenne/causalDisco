# automatically forbid edges that go from later tiers to earlier tiers
data(tpc_example)

kn <- knowledge(
  head(tpc_example),
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

# turn all tier violations to forbidden edges
kn2 <- convert_tiers_to_forbidden(kn)

print(kn)
print(kn2)
