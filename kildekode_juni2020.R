#wd
setwd("D:/Data/workdata/707372/Anne")

#settings
options(scipen = 999)


#source disco functions
source("./R/pcalgfunctions.R")

#Load datasets: death and dep
#load("./data/example_deathData.rda")
load("./data/example_depData.rda")

#Ordering of prefixes
vOrder <- c("birth", "child", "youth", "adult", "elderly")

#Labels
labs <- list(`birth_weight` = "Weight",
             `birth_length` = "Length",
             `birth_mother_married` = "Mother married",
             `birth_father_socialclasslow` = "Father low social class",
             `child_iq` = "Intelligence score",
             `child_creativity` = "Creativity score",
             `child_father_socialclasslow` = "Father low social class",
             `child_likeschool` = "Positive towards school",
             `child_mother_smoke` = "Maternal smoking",
             `child_father_smoke` = "Paternal smoking",
             `child_bullied` = "Bullied",
             `adult_no_children` = "Number of children",
             `adult_cohabit` = "Cohabitation",
             `adult_see_family` = "See family weekly",
             `adult_see_friends` = "See friends weekly",
             `adult_contact_family` = "In contact with family weekly",
             `adult_contact_friends` = "In contact with friends weekly",
             `adult_own_housing` = "Own housing",
             `adult_own_car` = "Own car",
             `adult_own_summerhouse` = "Own summerhouse",
             `adult_smoke_now` = "Smoke now",
             `adult_smoke_years` = "Total years of smoking",
             `adult_bmi` = "BMI",
             `adult_alco_bingeepisodes` = "Alcohol binging frequency",
             `adult_allteethleft` = "Still have all teeth",
             `adult_education_undergrad` = "Undergraduate education",
             `adult_employed` = "Employment status",
             `youth_cognition` = "Intelligence score",
             `youth_height` = "Height",
             `youth_bmi` = "BMI",
             `adult_income` = "Disposable income",
             `adult_depression_any` = "Depression")

#labsDeath <- c(labs, `elderly_death` = "Death")

labsDep <- c(labs, `elderly_depression_any` = "Depression")



########################################################################################
#Exclude and count
########################################################################################

#copy data
depdata <- depData

#exclude participants with missing midwife info
n_nomidwifeinfo <- sum(is.na(depdata$birth_mother_married) & 
                         is.na(depdata$birth_length) & 
                         is.na(depdata$birth_weight))
depdata <- depdata[!(is.na(depdata$birth_mother_married) & 
                       is.na(depdata$birth_length) & 
                       is.na(depdata$birth_weight)),]

#exclude participants that were not in the school survey
n_notinschoolsurvey <- sum(is.na(depdata$child_creativity) & is.na(depdata$child_iq)
                           & is.na(depdata$child_likeschool))
depdata <- depdata[!(is.na(depdata$child_creativity) & is.na(depdata$child_iq)
                     & is.na(depdata$child_likeschool)), ]

#exclude participants that do not have conscription data
n_noconscription <- sum(is.na(depdata$youth_bmi) & is.na(depdata$youth_cognition) &
                          is.na(depdata$youth_height))
depdata <- depdata[!(is.na(depdata$youth_bmi) & is.na(depdata$youth_cognition) &
                       is.na(depdata$youth_height)),]

#count and exclude emigrants
n_full <- nrow(depdata)
n_emi <- sum(depdata$emi_2004_2018)
depdata <- depdata[depdata$emi_2004_2018 == 0, setdiff(names(depdata), "emi_2004_2018")]

#count and exclude dead
n_dead <- sum(depdata $dead_18) 
depdata <- depdata[depdata$dead_18 == 0, setdiff(names(depdata), "dead_18")]

#Count and exclude missing information
depdatawmiss <- depdata
n_missing <- sum(!complete.cases(depdata))
depdata <- na.omit(depdata)

#overview
dropout <- list(`start n` = nrow(depData), 
                `n no midwife info` = n_nomidwifeinfo, 
                `n no school info` = n_notinschoolsurvey,
                `n no conscription info` = n_noconscription, 
                `n full`= n_full,
                `n emigrated` = n_emi, 
                `n dead`= n_dead, 
                `n missing info` = n_missing, 
                `n final`= nrow(depdata))


########################################################################################
#Descriptive statistics
########################################################################################

library(Publish)
library(xtable)

for (i in 1:ncol(depdatawmiss)) {
  thisv <- depdatawmiss[, i]
  if (length(na.omit(unique(thisv))) == 2) {
    levs <- levels(factor(thisv))
    if (identical(levs, c(0,1))) levs <- c(1,0)
    depdatawmiss[, i] <- factor(thisv, levels = levs)
  }
}

tab1 <- summary(univariateTable(as.formula(paste("~ ", paste(names(depdatawmiss), collapse = "+"))),
                                data =depdatawmiss))

xtable(tab1)

xtable(data.frame(dropout))

########################################################################################
#Complete case analysis
########################################################################################

#deathdata <- na.omit(deathData)

alphas <- rev(10^c(-2:-10))
nm <- length(alphas)
#death_skels <- 
dep_skels <- list()
#death_ms <- 
dep_ms <- list()
comptimes <- rep(NA, nm)


##CHANGE THIS AGAIN
#for (i in 1:nm) {
for (i in 9) {
  t0 <- Sys.time()
  thisalpha <- alphas[i]
  thisdepskel <- skeleton(suffStat = makeSuff2(depdata, order = vOrder), 
                          indepTest = gamTest2,
                          alpha = thisalpha, labels = names(depdata), 
                          method = "stable.fast")
  # thisdeathskel <- skeleton(suffStat = makeSuff2(deathdata, order = vOrder), 
  #                            indepTest = gamTest2,
  #                            alpha = thisalpha, labels = names(deathdata), 
  #                            method = "stable.fast")
  #  death_skels <- c(death_skels, list(thisdeathskel))
  dep_skels <- c(dep_skels, list(thisdepskel))
  
  comptimes[i] <- Sys.time() - t0
  
  # save(list = c("death_skels", "dep_skels"), file = "./results/article1_skels.rda")
  save(list = c("dep_skels"), file = "./results/article1_v2_skels.rda")
  save(list = c("comptimes"), file = "./results/article1_v2_comptimes.rda")
  
  
  cat(paste("alpa = ", thisalpha, "done."))
  gc()
}


#change this again!!!!!!!!
#for (i in 1:nm) {
for (i in 9) {
  thisalpha <- alphas[i]
  thisdepskel <- dep_skels[[i]]
  # thisdeathskel <- death_skels[[i]]
  
  thisdepm <- tpdag(thisdepskel, order = vOrder, vnames = names(depdata))
  # thisdeathm <- tpdag(thisdeathskel, order = vOrder, vnames = names(deathdata))
  
  # death_ms <- c(death_ms, list(thisdeathm))
  dep_ms <- c(dep_ms, list(thisdepm))
  
  #save(list = c("death_ms", "dep_ms"), file = "./results/article1_models.rda")
  
  save(list = c("dep_ms"), file = "./results/article1_v2_models.rda")
  
}


############################################################################################
######plotting

load("./results/article1_v2_skels.rda")
load("./results/article1_v2_models.rda")
load("./results/article1_v2_comptimes.rda")

usedeplabs <- labsDep[names(depdata)]
periodlabs <- c("Birth", "Childhood", "Youth", "Adulthood", "Early old age")
#usedeathlabs <- labsDeath[names(deathdata)]

#deathdata
#outcome <- "death"
#for (i in 1:length(alphas)) {
#  type <- "skeleton"
#  png(paste("./plots/article1/", type, outcome, i, ".png", sep = ""), 
#      width = 1000, height = 800, pointsize = 14)
#  plotOrderedAmat(amat(death_skels[[i]]), order = vOrder, data = deathdata,
#                  labels = usedeathlabs,
#                  asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
#                  colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
#                  sub = paste("psi = ", alphas[i], sep = ""),
#                  main ="Statistical life course model: death")
#  dev.off()
#  
#  type <- "tpdag"
#  png(paste("./plots/article1/", type, outcome, i, ".png", sep = ""), 
#      width = 1000, height = 800, pointsize = 14)
#  plotOrderedAmat(death_ms[[i]], order = vOrder, data = deathdata,
#                  labels = usedeathlabs,
#                  asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
#                  colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
#                  sub = paste("psi = ", alphas[i], sep = ""),
#                  main ="Causal life course model: death")
#  dev.off()
#}


#depdata
outcome <- "depression"
for (i in 1:length(alphas)) {
  #type <- "skeleton"
  #png(paste("./plots/article1/", type, outcome, i, ".png", sep = ""), 
  #    width = 1000, height = 800, pointsize = 14)
  #par(mar = c(0,0,0,0))
  #plotOrderedAmat(amat(dep_skels[[i]]), order = vOrder, data = depdata,
  #                labels = usedeplabs,
  #                asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
  #                colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
  #             #   sub = bquote(psi == .(alphas[i])),
  #                mark.border = c(NA, NA, NA, NA, "black"))
  #text(x = -1, y = 1, labels = bquote(psi == .(alphas[i])))
  ##rect(xleft = 0.3, xright = 0.8, ybottom = -1.1, ytop = 1.1)
  #dev.off()
  
  type <- "tpdag"
  png(paste("./plots/article1/", type, outcome, i, ".png", sep = ""), 
      width = 1000, height = 800, pointsize = 14)
  #par(mar = c(0,0,0,0))
  #  plotOrderedAmat(dep_ms[[i]], order = vOrder, data = depdata,
  #                  labels = usedeplabs,
  #                  asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
  #                  colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
  #                #  sub = bquote(psi == .(alphas[i])),
  #                  mark.border = c(NA, NA, NA, NA, "black")) 
  #  text(x = -1, y = 1, labels = bquote(psi == .(alphas[i])))
  #  
  #  
  #  
  plotOrderedAmat(amat(dep_ms[[i]]), order = vOrder, data = depdata,
                  labels = usedeplabs,
                  asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
                  colors = rev(RColorBrewer::brewer.pal(5,"Set1")))
  text(x = -0.75, y = 0.95, labels = bquote(psi == .(alphas[i])), cex = 2)
  axis(1, seq(-1, 1, 0.5),periodlabs, cex.axis = 1.5)
  dev.off()
}

#number of tests
data.frame(alpha = alphas,
           # ntestdeath = sapply(death_skels, function(x) sum(x@n.edgetests)),
           ntestdep = sapply(dep_skels, function(x) sum(x@n.edgetests))
)


#regular skeleton + cpdag for depression, alpha = 0.00001 (alphas[6])

#reg_depskel <- skeleton(suffStat = makeSuff2(depdata, order = vOrder), 
#                        indepTest = gamTest2,
#                        alpha = alphas[6], labels = names(depdata), 
#                        method = "stable.fast"#,
#                        #verbose = TRUE
#)
#png("./plots/article1/skeletondepression0.png", 
#    width = 1000, height = 800, pointsize = 14)
#plotOrderedAmat(amat(reg_depskel), order = vOrder, data = depdata,
#                labels = usedeplabs,
#                asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
#                colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
#                sub = paste("psi = ", alphas[6], sep = ""),
#                main ="Skeleton estimated w/o temporal information")
#dev.off()
#
#
#reg_depm <- udag2pdag(reg_depskel)
#png("./plots/article1/tpdagdepression0.png", 
#    width = 1000, height = 800, pointsize = 14)
#plotOrderedAmat(amat(reg_depm), order = vOrder, data = depdata,
#                labels = usedeplabs,
#                asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
#                colors = c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato"),
#                sub = paste("psi = ", alphas[6], sep = ""),
#                main ="CPDAG estimated w/o temporal information")
#dev.off()

#trycols <- c("steelblue1", "seagreen3", "yellowgreen", "orange", "tomato")
#library("RColorBrewer")
#display.brewer.all(n = 5)
#periodlabs <- c("Birth", "Childhood", "Youth", "Adulthood", "Early old age")
#
#trycols <- rev(RColorBrewer::brewer.pal(5,"Set1"))
#rcartocolor::carto_pal(5,"Bold")
#  jcolors::jcolors("default")[1:5]
#rev(ggsci::pal_locuszoom()(5))
#brewer.pal(5, "Dark2")
#plotOrderedAmat(amat(dep_skels[[1]]), order = vOrder, data = depdata,
#                labels = usedeplabs,
#                asp = 0.8, edge.arrow.size = 1.2, vertex.label.font = 2,
#                colors = rev(RColorBrewer::brewer.pal(5,"Set1")))
#text(x = -1, y = 1, labels = bquote(psi == .(alphas[1])), cex = 2)
#axis(1, seq(-1, 1, 0.5),periodlabs, pch = 2, cex = 2)
#arrows(-1.2, -1.11, 1.2, -1.11)
#