# generate data
devtools::load_all()
set.seed(123)

# available engines for pc
available_engines(pc)

# number of samples
n <- 10**4

# continuous data example
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

# binary example
B1 <- rbinom(n, 1, 0.5)
B2 <- rbinom(n, 1, 0.5 * B1)
B3 <- rbinom(n, 1, 1 - 0.3 * B1)
B4 <- 1 - B3

df_binary <- data.frame(B1, B2, B3, B4)

my_pc_g_square <- pc(engine = "pcalg", test = "g_square", alpha = 0.05)
disco(df_binary, method = my_pc_g_square)


# discrete example
D1 <- rbinom(n, 2, 0.5)
D2 <- rbinom(n, 2, 0.5 * D1)
D3 <- rbinom(n, 2, 1 - 0.3 * D1)
D4 <- 2 - D3

df_discrete <- data.frame(D1, D2, D3, D4)
disco(df_discrete, method = my_pc_g_square)
