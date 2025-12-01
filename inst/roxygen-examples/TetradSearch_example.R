### tetrad_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Requires Tetrad to be installed
\dontrun{
# Load data
set.seed(1405)
data("tpcExample")
small_dataset <- tpcExample[1:100, ]

# Recommended:
my_pc <- pc(engine = "tetrad", test = "conditional_gaussian")
my_pc(small_dataset)

# or
disco(data = small_dataset, method = my_pc)

# Using R6 class:
s <- TetradSearch$new()

s$set_data(small_dataset)
s$set_test(method = "conditional_gaussian", alpha = 0.05)
s$set_alg("pc")

g <- s$run_search()

print(g)
}
