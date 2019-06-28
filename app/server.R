library(shiny)
library(markdown)

propNames <- read.csv("propertyLabels.csv",
                      stringsAsFactors = FALSE)
descriptions <- read.csv("descriptions.csv",
                         stringsAsFactors = FALSE,
                         nrows = 24)

source("procedureChooser.R")

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
  
    output$distPlot <- renderPlot({
    
    hist(rnorm(100), breaks = 10, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$procedures <- renderUI({
    useProps <- propNames$assumption %in% input$inputProps
    pros <- procedureChooser(useProps)
    outids <- descriptions[descriptions$ID %in% pros, "ID"]
    outnames <- descriptions[descriptions$ID %in% pros, "name"]
    nOut <- length(outids)
    
    if (nOut == 0) return("No available procedures found.")
    
    outLinks <- lapply(1:nOut, function(x) {
      actionLink(inputId = paste("tile_", outids[x], sep = ""),
                 label = outnames[x])
    })
  })
  
  makeReactiveBinding("showProcedure")
  showProcedure <- "blank"
  
  observeEvent(input$tile_1, showProcedure <<- "tile_1")
  observeEvent(input$tile_2, showProcedure <<- "tile_2")
  observeEvent(input$tile_3, showProcedure <<- "tile_3")
  observeEvent(input$tile_4, showProcedure <<- "tile_4")
  observeEvent(input$tile_5, showProcedure <<- "tile_5")
  observeEvent(input$tile_6, showProcedure <<- "tile_6")
  observeEvent(input$tile_7, showProcedure <<- "tile_7")
  observeEvent(input$tile_8, showProcedure <<- "tile_8")
  observeEvent(input$tile_9, showProcedure <<- "tile_9")
  observeEvent(input$tile_10, showProcedure <<- "tile_10")
  observeEvent(input$tile_11, showProcedure <<- "tile_11")
  observeEvent(input$tile_12, showProcedure <<- "tile_12")
  observeEvent(input$tile_13, showProcedure <<- "tile_13")
  observeEvent(input$tile_14, showProcedure <<- "tile_14")
  observeEvent(input$tile_15, showProcedure <<- "tile_15")
  observeEvent(input$tile_16, showProcedure <<- "tile_16")
  observeEvent(input$tile_17, showProcedure <<- "tile_17")
  observeEvent(input$tile_18, showProcedure <<- "tile_18")
  observeEvent(input$tile_19, showProcedure <<- "tile_19")
  observeEvent(input$tile_20, showProcedure <<- "tile_20")
  observeEvent(input$tile_21, showProcedure <<- "tile_21")
  observeEvent(input$tile_22, showProcedure <<- "tile_22")
  observeEvent(input$tile_23, showProcedure <<- "tile_23")
  observeEvent(input$tile_24, showProcedure <<- "tile_24")
  
  output$whichproc <- renderUI({
    includeHTML(paste("www/", showProcedure, ".html", sep = ""))
  })
}

