#Load numeric dataset numData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData.rda"))

#Load package
library(pcalg)

#Prepare data for pc() call 
##Note: this choice of sufficient statistics is valid for Gaussian data.
pcalg_suffstat_numData <- list(C = cor(numData), n = nrow(numData))

#Learn causal structure using pc()
#Mandatory arguments: 
##Sufficient statistic (pcalg_suffstat_numData).
##Labels, i.e. names for variables. We use the names from the data.frame.
##Independence test, which function to use for performing conditonal
###independe tests. We use a test suited for Gaussian data
##The significance level (alpha) for the independence tests. We use
###a significance level of 1%.
pcalg_pc_out <- pc(pcalg_suffstat_numData, labels = names(numData),
                   indepTest = gaussCItest, alpha = 0.01)

#Summary of output
summary(pcalg_pc_out)

#Plot output
plot(pcalg_pc_out)


