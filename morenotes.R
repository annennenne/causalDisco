source("R/tpc.R")
source("R/misc.R")
source("R/plot.R")
source("R/regTest.R")
source("R/corTest.R")
library(splines)
library(pcalg)

###a
set.seed(123)
n <- 500

child_x <- rnorm(n)^2
child_y <- 0.5*child_x + rnorm(n)
child_z <- sample(c(0,1), n, replace = TRUE, prob = c(0.3, 0.7))
adult_x <- child_x + rnorm(n)
adult_z <- as.numeric(child_z + rnorm(n) > 0)
adult_w <- 2*adult_z + rnorm(n)
adult_y <- 2*sqrt(child_x) + adult_w^2 + rnorm(n)

simdata <- data.frame(child_x, child_y, child_z,
                      adult_x, adult_z, adult_w,
                      adult_y)
head(simdata)
#> rbind(aa@graph@nodes, 1:7)
#
#[1,] "child_x" "child_y" "child_z" "adult_x" "adult_z" "adult_w" "adult_y"
#[2,] "1"       "2"       "3"       "4"       "5"       "6"       "7"


#m <- glm(adult_w ~ ns(adult_y, df = 3) + ns(child_x, df = 3),
#         data = simdata)
#
#drop1(m, test = "LRT")

simorder <- c("child", "adult")
head(simdata)
#pairs(simdata)
nres1 <- tpc(simdata, simorder, 10^(-2),
           test = regTest)

nres1
plot(nres1)

nres2 <- tpc(simdata, simorder, 10^(-10),
           test = regTest)

#plotOrderedAmat(amat(tpdag(res, order = simorder, vnames = names(simdata))), order = simorder)
nres2
plot(nres2)


nres3 <- tpc(simdata, simorder, 10^(-1),
            test = corTest)
nres3

plot(nres3)


#save(list = c("res1", "res2", "res3"), file = "res_a_oldtpc201020.rda")


aa <- pc(list(C = cor(simdata), n = nrow(simdata)),
    labels = names(simdata), indepTest = gaussCItest,
    alpha = 10^(-2))
aa
plot(aa)

###a2
set.seed(123)
n <- 100

child_x <- sample(c(0,1), n, replace = T)
child_y <- as.numeric(0.5*child_x + rnorm(n) > 0.5)
child_z <- sample(c(0,1), n, replace = TRUE, prob = c(0.3, 0.7))
adult_x <- as.numeric(child_x + rnorm(n) > 0.6)
adult_z <- as.numeric((child_z + rnorm(n, mean = -0.5)) > 0)
adult_w <- as.numeric(2*adult_z + rnorm(n) > 1)
adult_y <- as.numeric(sqrt(child_x) + adult_w^2 + rnorm(n) > 0)

simdata <- data.frame(child_x, child_y, child_z,
                      adult_x, adult_z, adult_w,
                      adult_y)

simorder <- c("child", "adult")
head(simdata)


res <- tpc(simdata, simorder, 10^(-1),
           test = gamTest)
res

plot(res)

res2 <- tpc(simdata, simorder, 10^(-1),
            test = gamTest)
res2

plot(res2)






#########################################################
##B

set.seed(123)
n <- 1000

child_y <- sample(c(0,1), n, replace = TRUE)
child_x <- 2*child_y + rnorm(n)
youth_z <- sample(c(0,1), n, replace = TRUE)
youth_y <- child_y + youth_z + rnorm(n, sd = 0.1)
adult_x <- child_x^2 - child_x + rnorm(n)
adult_z <- 2*youth_z^2 - adult_x + rnorm(n)

minidata <- data.frame(child_x, child_y,
                       youth_y, youth_z,
                       adult_x,adult_z)
miniorder <- c("child", "youth", "adult")
head(minidata)
#pairs(minidata)

mres <- tpc(minidata, miniorder, sparsity = 10^(-1),
            test = gamTest_actual)

plot(mres)

mres


mres2 <- tpc(minidata, miniorder, sparsity = 10^(-1),
             test = gamTest)

plot(mres2)
mres2
