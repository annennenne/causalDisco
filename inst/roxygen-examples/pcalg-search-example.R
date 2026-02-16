### pcalg_search R6 class examples ###

# Generally, we do not recommend using the R6 classes directly, but rather
# use the disco() or any method function, for example pc(), instead.

# Load data
data(num_data)

# Recommended:
my_pc <- pc(engine = "pcalg", test = "fisher_z")
my_pc(num_data)

# or
disco(data = num_data, method = my_pc)

# Example with detailed settings:
my_pc2 <- pc(
  engine = "pcalg",
  test = "fisher_z",
  alpha = 0.01,
  m.max = 4,
  skel.method = "original"
)

disco(data = num_data, method = my_pc2)

# With knowledge

kn <- knowledge(
  num_data,
  X1 %!-->% X2,
  X2 %!-->% X1
)

disco(data = num_data, method = my_pc2, knowledge = kn)

# Using R6 class:
s <- PcalgSearch$new()

s$set_test(method = "fisher_z", alpha = 0.05)
s$set_data(tpc_example)
s$set_alg("pc")

g <- s$run_search()

print(g)
