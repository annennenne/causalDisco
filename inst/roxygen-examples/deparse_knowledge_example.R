# turn a knowledge object back into DSL code
data(tpc_example)

kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  ),
  child_x1 %-->% youth_x3,
  forbidden(oldage_x6 ~ child_x1)
)

kn <- forbid_tier_violations(kn)

code <- deparse_knowledge(kn, df_name = "tpc_example")

cat(code)
# printed output:
# knowledge(tpc_example,
#   tier(
#     child ~ child_x1 + child_x2,
#     youth ~ youth_x3 + youth_x4,
#     old ~ oldage_x5 + oldage_x6
#   ),
#   forbidden(
#     oldage_x5 ~ child_x1 + child_x2 + youth_x3 + youth_x4,
#     oldage_x6 ~ child_x1 + child_x2 + youth_x3 + youth_x4,
#     youth_x3 ~ child_x1 + child_x2,
#     youth_x4 ~ child_x1 + child_x2
#   ),
#   required(
#     child_x1 ~ youth_x3
#   )
# )
