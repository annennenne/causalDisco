#Load numeric dataset with latent confounding numData_latent
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_numData_latent.rda"))

#Load package
library(pcalg)

#Prepare data for fci() call 
##Note: this choice of sufficient statistics is valid for Gaussian data.
pcalg_suffstat_numData_latent <- list(C = cor(numData_latent), 
                                      n = nrow(numData_latent))

#Learn causal structure using rfci()
#Mandatory arguments: 
##Sufficient statistic (pcalg_suffstat_numData_latent).
##Labels, i.e. names for variables. We use the names from the data.frame.
##Independence test, which function to use for performing conditonal
###independe tests. We use a test suited for Gaussian data
##The significance level (alpha) for the independence tests. We use
###a significance level of 1%.
pcalg_rfci_out <- rfci(pcalg_suffstat_numData_latent, 
                       labels = names(numData_latent),
                       indepTest = gaussCItest, alpha = 0.01)

#Summary of output
summary(pcalg_rfci_out)

#Plot output
plot(pcalg_rfci_out)


