#load("./data/exampledata_numData.rda")
#head(numData)
##C

set.seed(123)
n <- 10000

child_x <- rnorm(n, sd = 1)
child_y <- child_x^2 + rnorm(n, sd = 1)
youth_x <- child_x^2 + rnorm(n, sd = 1)
youth_y <- sample(c(0,1), n, replace = TRUE)
adult_y <- youth_y^3 + child_x + rnorm(n, sd = 1)
adult_x <- youth_x^2 + rnorm(n, sd = 1)
adult_z <- adult_x + adult_y^2 + rnorm(n, sd = 1)

toydata <- data.frame(child_x, child_y, youth_x, youth_y, adult_x, adult_y, adult_z)
#toydata <- as.data.frame(scale(data.frame(child_x, child_y, youth_x, youth_y, adult_x, adult_y, adult_z)))
toyorder <- c("child", "youth", "adult")
head(toydata)
pairs(toydata)

#library(causalDisco)
#res <- tpc(toydata, order = toyorder, sparsity = 10^(-1), dfs = 0)
#res1 <- tpc(toydata, order = toyorder, sparsity = 10^(-1),
#            test = causalDisco:::gamTest)
res2 <- tpc(toydata, order = toyorder, sparsity = 10^(-1),
            test = gamTest_actual)
plot(res2)

#res1
#res2
#plot(res1)



#######


set.seed(123)
n <- 1000

child_x2 <- rnorm(n, sd = 1)
child_y2 <- child_x2 + rnorm(n, sd = 1)
youth_x2 <- child_x2 + rnorm(n, sd = 1)
youth_y2 <- rnorm(n)
adult_y2 <- youth_y2 + child_x2 + rnorm(n, sd = 1)
adult_x2 <- youth_x2 + rnorm(n, sd = 1)
adult_z2 <- adult_x2 + adult_y2 + rnorm(n, sd = 1)

toydata2 <- data.frame(child_x2, child_y2, youth_x2, youth_y2,
                       adult_x2, adult_y2, adult_z2)

res12 <- tpc(toydata2, order = toyorder, sparsity = 10^(-1),
            test = causalDisco:::gamTest)
res22 <- tpc(toydata2, order = toyorder, sparsity = 10^(-1),
            test = causalDisco:::gamTest_actual)
res12
res22

















library(pcalg)
aa <-pc(list(C = cor(toydata), n = nrow(toydata)),
          labels = names(toydata), indepTest = gaussCItest,
             alpha = 10^(-2))
aa
plot(aa)

head(res)

plot(res)

debugonce("plot.tpdag")


causalDisco:::plotOrderedAmat(causalDisco:::amat(res), order = toyorder, data = toydata)


debugonce(causalDisco:::plotOrderedAmat)




foo <- function(x) {
  plot(1:10, 1:10)
  abline(1,0)
}

foo()

ss <- sciNotation(0.00001)
plot(1:10)
mtext(substitute(paste(expression(psi, paste("= ", ss)))), side = 3)

plot(1:10, xlab = expression(psi ~ eval(substitute(ss))),
     ylab = expression("Temperature (" * degree * C *")"))
