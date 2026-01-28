################# Convert Knowledge to Tikz ################

data(num_data)
kn <- knowledge(
  num_data,
  X1 %-->% X2,
  X2 %!-->% c(X3, Y),
  Y %!-->% Z
)

plot_kn <- plot(kn)

# Full standalone document
tikz_kn <- make_tikz(plot_kn, scale = 10, full_doc = TRUE)
cat(tikz_kn)

# Only the tikzpicture environment
tikz_kn_snippet <- make_tikz(plot_kn, full_doc = FALSE)
cat(tikz_kn_snippet)

# With bent edges
tikz_bent <- make_tikz(
  plot_kn,
  full_doc = FALSE,
  bend_edges = TRUE
)
cat(tikz_bent)

# With a color not supported by default TikZ colors
plot_darkblue <- caugi::plot(kn, node_style = list(fill = "darkblue"))
tikz_darkblue <- make_tikz(plot_darkblue, full_doc = FALSE)
# Will use RGB specification for darkblue
cat(tikz_darkblue)

# With tiered knowledge
data(tpc_example)
kn <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)
plot_tiered_kn <- plot(kn)
tiers <- list(
  child = c("child_x1", "child_x2"),
  youth = c("youth_x3", "youth_x4"),
  old = c("oldage_x5", "oldage_x6")
)
tikz_tiered_kn <- make_tikz(
  plot_tiered_kn,
  tier_node_map = tiers,
  full_doc = FALSE
)
cat(tikz_tiered_kn)


################# Convert Knowledgeable Caugi to Tikz ################

data(num_data)
kn <- knowledge(
  num_data,
  X1 %-->% X2,
  X2 %!-->% c(X3, Y),
  Y %!-->% Z
)

pc_bnlearn <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
disco_kn <- disco(data = num_data, method = pc_bnlearn, knowledge = kn)

disco_plot <- plot(disco_kn)
tikz_snippet <- make_tikz(disco_plot, scale = 10, full_doc = FALSE)
cat(tikz_snippet)

################# Convert caugi objects to Tikz ################

cg <- caugi::caugi(A %-->% B + C)
plot_obj <- caugi::plot(cg, node_style = list(fill = "red"))

tikz_snippet <- make_tikz(plot_obj, scale = 10, full_doc = FALSE)
cat(tikz_snippet)
