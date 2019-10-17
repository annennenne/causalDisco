library(causalDisco)

data("exampledata_numData")
numData <- exampledata_numData
View(numData)


obs1 <- Observations$new(data = numData)
obs1$set_properties(DataProperty$new("num data"))
obs1$get_properties()$get()

inv1 <- Investigation$new(observations = Observations$new(data = numData))


inv1$get_log()
inv1$get_theory()$get_assumptions()
inv1$get_observations$get_properties()
inv1$log_add_comment("hi tehre")

