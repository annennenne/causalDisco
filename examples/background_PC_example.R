# generate data
devtools::load_all()
set.seed(123)

n <- 10**4

V1 <- rnorm(n, 0, 1)
V2 <- 0.5 * V1 + rnorm(n, 0, 0.5)
V3 <- V2 + rnorm(n, 0, 0.1)
V4 <- V3 + rnorm(n, 0, 1)
V5 <- rnorm(n, 0, 1)
V6 <- rnorm(n, 0, 1) + 0.7 * V5

df <- data.frame(V1, V2, V3, V4, V5, V6)

# tiers
# tier 1: V1, V2, V3, V4
# tier 2: V5, V6

# Create a knowledge object
knowledge <- knowledge_tetrad(
  tier(
    1, c("V1", "V2", "V3"),
    2, c("V4", "V5", "V6")
  ),
  forbidden("V1", "V6"), # forbidden and required can be called like this
  forbidden(c("V2", "V6")), # or like this
  required(c("V1", "V2"), c("V2", "V3")) # or like this
)


# disco call example
my_pc <- pc(engine = "tetrad", test = "fisher_z", alpha = 0.05)

disco(df, method = my_pc, knowledge = knowledge) |> cat()
