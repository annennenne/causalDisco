library(shiny)

propNames <- read.csv("propertyLabels.csv",
                      stringsAsFactors = FALSE)

ui <- fluidPage(
  
  # App title ----
  fluidRow(titlePanel(img(src="https://github.com/annennenne/causalDisco/raw/master/webtool/app/www/logo.png", 
                          align = "left", width = "50%"),
             windowTitle = "causalDisco")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      checkboxGroupInput("inputProps", "Choose properties",
                         choiceNames = propNames$label,
                         choiceValues = propNames$assumption),
      HTML("<b>Available procedures:</b>"),
      uiOutput("procedures")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      fluidRow(tabsetPanel(
        tabPanel("Procedure information", 
                 htmlOutput("whichproc")
                ),
        tabPanel("About the data",
                 includeHTML("www/aboutdata.html")
                 ),
        tabPanel("About the tool", 
                 includeHTML("www/help.html")),
        selected = "Procedure information"
        )
      )
    )
  )
)
