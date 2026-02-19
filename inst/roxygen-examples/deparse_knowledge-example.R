# turn a Knowledge object back into DSL code
data(tpc_example)

kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  ),
  child_x1 %-->% youth_x3,
  oldage_x6 %!-->% child_x1
)

code <- deparse_knowledge(kn, df_name = "tpc_example")
cat(code)

# Explicitly add all forbidden edges implied by tiers
kn <- convert_tiers_to_forbidden(kn)
code <- deparse_knowledge(kn, df_name = "tpc_example")
cat(code)
