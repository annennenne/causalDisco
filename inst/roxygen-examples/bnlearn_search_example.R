### bnlearn_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data("asia", package = "bnlearn")

# Recommended:
pc(engine = "bnlearn", test = "mi", alpha = 0.05)(asia)

# or
my_pc <- pc(engine = "bnlearn", test = "mi", alpha = 0.05)
my_pc(asia)

# or
disco(data = asia, method = my_pc)

# Using R6 class:
s <- bnlearnSearch$new()

s$set_data(asia)
s$set_test(method = "mi", alpha = 0.05)
s$set_alg("pc")

g <- s$run_search()

print(g)
