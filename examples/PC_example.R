# generate data
set.seed(123)
n <- 100
p <- 5
dag <- matrix(0, p, p)
dag[1, 2] <- 1
dag[1, 3] <- 1
dag[2, 4] <- 1
dag[3, 4] <- 1
dag[4, 5] <- 1
cov <- matrix(0.5, p, p)
diag(cov) <- 1
data <- MASS::mvrnorm(n, rep(0, p), cov)
df <- data.frame(data)
colnames(df) <- paste0("V", 1:p)

# pc example
ts_obj <- TetradSearch$new(df)
ts_obj$use_fisher_z()
ts_obj$run_pc() #
ts_obj$get_dot() |> cat()

# fges example
ts_obj2 <- TetradSearch$new(df)
ts_obj2$use_sem_bic(penalty_discount = 2, structure_prior = 0, sem_bic_rule = 1)
ts_obj2$run_fges()
ts_obj2$get_dot() |> cat()
