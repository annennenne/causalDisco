#Load categorical dataset catData
load(url("https://github.com/annennenne/causalDisco/raw/master/webtool/data/exampledata_catData.rda"))

#Load package
library(bnstruct)

#Prepare data for learn.network() call: convert data into bnstruct BNDataset format
##Mandatory arguments:
###A dataset consisting of numbers. It will *not* be treated as numerical, however,
####so using arbitrary numerical values to represent factor level is okay. 
###discreteness: Vector of logicals indicating whether each variable is
####categorical or not. We use TRUE for all. Note that if non-categorical variables
####are supplied, they are discretized before being used further.
###variables: the names of the variables (we use the names from the dataset)
###node.sizes: number of unique levels for each variable. We specify 5 for all of them,
####since they all have 5 levels.
catData_asNumbers <- as.data.frame(sapply(catData, as.numeric))
bnstruct_catData <- BNDataset(catData_asNumbers, 
                              discreteness = rep(TRUE, 5),
                              variables = names(catData_asNumbers),
                              node.sizes = rep(5, 5))


#Learn causal structure using learn.network() with "mmhc" argument:
#Mandatory arguments: 
##The dataset as a BNDataset object
##algo: Which algorithm to use for structure learning. We choose "mmch"
###(Max-Min Hill-Climbing)
bnstruct_mmhc_out <- learn.network(bnstruct_catData, 
                                   algo = "mmhc")


#Outputted adjacency matrix 
  #Adjacency matrix without variable names
  bnstruct_mmhc_out_adjM <- bnstruct_mmhc_out@dag
  
  #Add variable names 
  rownames(bnstruct_mmhc_out_adjM) <- bnstruct_mmhc_out@variables
  colnames(bnstruct_mmhc_out_adjM) <- bnstruct_mmhc_out@variables
  
  #Look at adjacency matrix with names
  bnstruct_mmhc_out_adjM

#Plot output
plot(bnstruct_mmhc_out)

