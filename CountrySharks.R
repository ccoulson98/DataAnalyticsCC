##this app summarizes the total cases in each province in canada as well as in Canada from jan to may

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

# read data
sharks <- read_csv("sharks.csv")


## Create UI
ui <- fluidPage(
  titlePanel("Shark Attacks world wide"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Country",
        label = "Select Type of attack",
        choices = c("USA",               
 "AUSTRALIA",          
  "SOUTH AFRICA",       
  "PAPUA NEW GUINEA",   
  "BRAZIL",             
  "BAHAMAS",             
  "NEW ZEALAND",         
  "MEXICO",              
  "REUNION",     
"NEW CALEDONIA"
        )
      ),
      dateRangeInput(
        inputId = "Date",
        label = "Select date range",
        start = min(sharks$Date),
        end = max(sharks$Date)
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "sharks_plot")
    )
  )
)

##Create server
server <- function(input, output) {
  output$sharks_plot <- renderPlotly({
    p <- sharks %>%
      filter(Country == input$Country) %>%
      filter(between(Date, input$Date[1], input$Date[2])) %>%
      count(Date, Country, Type) %>%
      ggplot(aes(x = Type)) +
      geom_bar()
    
     
    
  })
}

# Shiny App
shinyApp(ui = ui, server = server)
