### bnlearn_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data(num_data)

# Recommended:
my_pc <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
result <- my_pc(num_data)

# or
result <- disco(data = num_data, method = my_pc)

plot(result)

# Example with detailed settings:
my_pc2 <- pc(
  engine = "bnlearn",
  test = "mi_g",
  alpha = 0.01
)
disco(data = num_data, method = my_pc2)

# Using R6 class:
s <- BnlearnSearch$new()

s$set_data(num_data)
s$set_test(method = "fisher_z", alpha = 0.05)
s$set_alg("pc")

g <- s$run_search()

plot(g)
