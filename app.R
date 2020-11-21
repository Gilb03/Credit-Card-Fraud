library(shiny)
library(shinydashboard)
setwd("~/zrc/credit-fraud/data")
cardData <- read.csv("creditcard.csv")

ui <- dashboardPage(
  dashboardHeader( title="Creditcard Fraud Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("correlation_plot"), width=8),
    box(
      selectInput("Variables", "Features:",
                  c("Time", "V2")), width=4
    )
  )
)

server <- function(input, output){
  output$correlation_plot <- renderPlot({
    plot(cardData$Time, cardData$V28)
    
  })
  
}

shinyApp(ui,server)
