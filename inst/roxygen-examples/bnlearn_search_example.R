### bnlearn_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

s <- bnlearnSearch$new()

# 1) Attach data
data("asia", package = "bnlearn")
s$set_data(asia)

# 2) Choose a CI test for discrete data
s$set_test(method = "mi", alpha = 0.05)

# 3) Select the PC algorithm
s$set_alg("pc")

# 4) Run the search
g <- s$run_search()

print(g)

# equivalently using causalDisco frontend
pc(engine = "bnlearn", test = "mi", alpha = 0.05)(asia)

# or
my_pc <- pc(engine = "bnlearn", test = "mi", alpha = 0.05)
my_pc(asia)

# or
disco(data = asia, method = my_pc)
