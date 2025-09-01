devtools::load_all()
set.seed(123)

# number of samples
n <- 10**4

# continuous data example
V1 <- rnorm(n, 0, 1)
V2 <- 0.5 * V1 + rnorm(n, 0, 0.5)
V3 <- V2 + rnorm(n, 0, 0.1)
V4 <- V3 + rnorm(n, 0, 1)
V5 <- rnorm(n, 0, 1)
V6 <- rnorm(n, 0, 1) + 0.7 * V5

df <- data.frame(V1, V2, V3, V4, V5, V6)

# create a knowledge object
kn <- knowledge(
  df,
  tier(
    baby ~ V1 + V2,
    child ~ V3,
    adult ~ V4,
    old ~ V5 + V6
  ),
  forbidden(V1 ~ V3),
  required(V1 ~ V2, V2 ~ V3)
)
kn2 <- knowledge(
  df,
  forbidden(V1 ~ V3),
  required(V1 ~ V2, V2 ~ V3)
)
my_tges_tetrad <- ges(engine = "tetrad", score = "sem_bic")
my_ges_pcalg <- ges(engine = "pcalg", score = "sem_bic", directed_as_undirected_knowledge = TRUE)
my_tges_causalDisco <- tges(engine = "causalDisco", score = "tbic")

# call disco with background knowledge
disco(df, method = my_tges_causalDisco, knowledge = kn)
disco(df, method = my_ges_pcalg, knowledge = kn2)
disco(df, method = my_tges_tetrad, knowledge = kn)
