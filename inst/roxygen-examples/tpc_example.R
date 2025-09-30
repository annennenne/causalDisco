### tpc() example ###

# Load data
data(tpcExample)

# Build knowledge
kn <- knowledge(
  tpcExample,
  tier(
    child ~ tidyselect::starts_with("child"),
    youth ~ tidyselect::starts_with("youth"),
    old ~ tidyselect::starts_with("old")
  )
)

# Recommended route using disco
my_tpc <- tpc(engine = "causalDisco", test = "fisher_z", alpha = 0.05)

disco(tpcExample, my_tpc, knowledge = kn)

# or using my_tpc directly

my_tpc <- my_tpc |> set_knowledge(kn)
my_tpc(tpcExample)

# Using tpc_run() directly

tpc_run(tpcExample, knowledge = kn, alpha = 0.01)

# Deprecated: using order prefixes (will warn)
testthat::expect_warning(
  tpc_run(tpcExample, order = c("child", "youth", "oldage"), alpha = 0.01)
)
