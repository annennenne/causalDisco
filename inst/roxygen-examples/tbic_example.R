# Simulate Gaussian data
set.seed(123)
n <- 500
child_x <- rnorm(n)
child_y <- 0.5 * child_x + rnorm(n)
child_z <- 2 * child_x + child_y + rnorm(n)

adult_x <- child_x + rnorm(n)
adult_z <- child_z + rnorm(n)
adult_w <- 2 * adult_z + rnorm(n)
adult_y <- 2 * child_x + adult_w + rnorm(n)

simdata <- data.frame(
  child_x, child_y, child_z,
  adult_x, adult_z, adult_w,
  adult_y
)

# Define order in prefix way
kn <- knowledge(
  simdata,
  tier(
    child ~ tidyselect::starts_with("child"),
    adult ~ tidyselect::starts_with("adult")
  )
)

# Define TBIC score
t_score <- new("TemporalBIC",
  knowledge = kn,
  data = simdata
)
# Run tges
tges_pre <- tges_run(t_score)

# Plot MPDAG
# plot(tges_pre)
