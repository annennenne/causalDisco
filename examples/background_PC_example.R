devtools::load_all()
set.seed(123)

# number of samples
n <- 10**4

# continuous data example
V1 <- rnorm(n, 0, 1)
V3 <- rnorm(n, 0, 0.1)
V2 <- 0.5 * V1 + 0.5 * V3
V4 <- V3 + rnorm(n, 0, 1)
V5 <- V3 + rnorm(n, 0, 1)
V6 <- rnorm(n, 0, 0.1) + 0.7 * V5

df <- data.frame(V1, V2, V3, V4, V5, V6)

# create a knowledge object
kn <- knowledge(
  df,
  # tier(
  #   baby ~ V1 + V2 + V3,
  #   adult ~ V4,
  #   old ~ V5 + V6
  # ),
  required(
    V1 ~ V2,
    V5 ~ V6
  )
)

# initialize pc algorithm as usual
my_pc_tetrad <- pc(
  engine = "tetrad", test = "fisher_z", alpha = 0.05,
)
my_pc_pcalg <- pc(
  engine = "pcalg", test = "fisher_z", alpha = 0.05,
  directed_as_undirected_knowledge = TRUE
)
my_pc_bnlearn <- pc(
  engine = "bnlearn", test = "fisher_z", alpha = 0.05
)

# can be called like this
my_pc_tetrad(df)
new_pc_tetrad <- my_pc_tetrad |>
  set_knowledge(kn)
new_pc_tetrad(df)

# with pcalg
my_pc_pcalg(df)
new_pc_pcalg <- my_pc_pcalg |>
  set_knowledge(kn)
new_pc_pcalg(df)

# with bnlearn
my_pc_bnlearn(df)
new_pc_bnlearn <- my_pc_bnlearn |>
  set_knowledge(kn)
new_pc_bnlearn(df)

# or with disco
# call disco with background knowledge
disco(df, method = my_pc_tetrad, knowledge = kn)
disco(df, method = my_pc_pcalg, knowledge = kn)
disco(df, method = my_pc_bnlearn, knowledge = kn)
