# generate data
devtools::load_all()
set.seed(123)

n <- 10**4
dag <- matrix(0, p, p)

V1 <- rnorm(n, 0, 1)
V2 <- 0.5 * V1 + rnorm(n, 0, 0.5)
V3 <- V2 + rnorm(n, 0, 0.1)
V4 <- V3 + rnorm(n, 0, 1)
V5 <- rnorm(n, 0, 1)
V6 <- rnorm(n, 0, 1) + 0.7 * V5

df <- data.frame(V1, V2, V3, V4, V5, V6)

# disco call example
my_pc <- pc(engine = "tetrad", test = "fisher_z")
disco(df, method = my_pc) |> cat()
