### tges_run() example ###

# Recommended route using disco:
kn <- knowledge(
  tpcExample,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

my_tges <- tges(engine = "causalDisco", score = "tbic")

disco(tpcExample, my_tges, knowledge = kn)

# another way to run it

my_tges <- my_tges |>
  set_knowledge(kn)
my_tges(tpcExample)


# or you can run directly with tges_run()

data("tpcExample")

score_bic <- new("TemporalBIC",
  data = tpcExample,
  nodes = colnames(tpcExample),
  knowledge = kn
)

res_bic <- tges_run(score_bic)
res_bic
