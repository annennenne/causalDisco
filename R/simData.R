############################################################################################
######################Simulate data to be used for causalDisco code examples################
############################################################################################
#We simulate five different datasets: 
##1. One with only numeric variables (numData)
##2. One with only categorical variables (catData)
##3. One with both numeric and categorical variables (mixData)
##4. One with only categorical variables and furthermore with some observations missing 
##    completely at random (catData_mcar)
##5. One with only numeric variables and furthermore one latent confounder (X3) that
##    is present in the data generating mechanism, but not in the data (numData_latent)
#All data sets use the numerical data as it offset. This dataset is simulated as 
#a mix of linear and non-linear structural equations with additive noise. The noise 
#components are Gaussian or uniform. 
#The datasets are stored in data.frames and converted in the code examples, if needed.

#sample size
n <- 1000 

#Simulate numData
set.seed(123)
numData <- data.frame(Z = abs(rnorm(n, mean = 10))) 
numData$X1 <- sqrt(numData$Z) + runif(n, min = 0, max = 2)
numData$X3 <- runif(n, min = 5, max = 10)
numData$X2 <- 2*numData$X3 - rnorm(n, mean = 5) 
numData$Y <- numData$X1^2 + numData$X2 - numData$X3 - numData$Z + rnorm(n, mean = 10)
numData <- numData[, c("X1", "X2", "X3", "Z", "Y")]

#Make catData
catData <- as.data.frame(sapply(numData, function(x) cut(x, breaks = 5,
                                                         labels = letters[1:5])))

#Make mixData
mixData <- numData
mixData$X1 <- catData$X1
mixData$X2 <- catData$X2
mixData$X3 <- catData$X3

#Make catData_mcar
catData_mcar <- catData
set.seed(1234)
catData_mcar$X1[sample(1:n, 100)] <- NA
catData_mcar$X2[sample(1:n, 50)] <- NA
catData_mcar$X3[sample(1:n, 200)] <- NA

#Make numData_latent
numData_latent <- numData[, c("X1", "X2", "Z", "Y")]

#Save datasets 
save(list = "numData", file = "./data/exampledata_numData.rda")
save(list = "catData", file = "./data/exampledata_catData.rda")
save(list = "mixData", file = "./data/exampledata_mixData.rda")
save(list = "catData_mcar", file = "./data/exampledata_catData_mcar.rda")
save(list = "numData_latent", file = "./data/exampledata_numData_latent.rda")

