# generate data
devtools::load_all()
set.seed(123)

n <- 10**4

V1 <- rnorm(n, 0, 1)
V2 <- 0.5 * V1 + rnorm(n, 0, 0.5)
V3 <- V2 + rnorm(n, 0, 0.1)
V4 <- V3 + rnorm(n, 0, 1)
V5 <- rnorm(n, 0, 1)
V7 <- rnorm(n, 0, 1) + 0.7 * V5

df <- data.frame(V1, V2, V3, V4, V5, V7)

# disco call example
my_pc <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
my_pc_tetrad <- pc(engine = "tetrad", test = "fisher_z", alpha = 0.05)

disco(df, method = my_pc)
disco(df, method = my_pc_tetrad)

# available engines
available_engines(pc)
