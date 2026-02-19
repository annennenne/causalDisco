# produce whitelist/blacklist data frame for bnlearn
data(tpc_example)

kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    oldage ~ starts_with("old")
  ),
  child_x1 %-->% youth_x3
)

bnlearn_kn <- as_bnlearn_knowledge(kn)
print(bnlearn_kn)
