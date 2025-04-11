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
  tier(
    1, c("V1", "V2", "V3"),
    2, c("V4", "V5", "V6")
  ),
  forbidden("V1", "V6"), # forbidden and required can be called like this
  forbidden(c("V2", "V6")), # or like this
  required(c("V1", "V2"), c("V2", "V3")) # or like this
)


# initialize fci algorithm
my_fci_tetrad <- fci(engine = "tetrad", test = "fisher_z", alpha = 0.05)

# call disco with background knowledge
disco(df, method = my_fci_tetrad, knowledge = kn)
