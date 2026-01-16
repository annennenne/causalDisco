#Load categorical dataset with missing information catData_mcar
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_catData_mcar.rda"))

#Load package
library(catnet)

#Learn causal structure using cnSearchSA() and cnFind()
#Mandatory arguments: 
##A dataset

  #Find candidate networks  
  catnet_allnets <- cnSearchSA(catData_mcar)
  
  #Select one network using BIC scoring
  catnet_out <- cnFind(catnet_allnets, alpha = "BIC")

  
#Print output
catnet_out

#Native plot method cnPlot() does not work. 

