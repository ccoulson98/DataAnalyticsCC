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
        inputId = "Type",
        label = "Select Type of attack",
        choices = c("Boat",           
 "Boating",        
 "Invalid",      
 "Provoked",       
 "Sea Disaster",   
 "Unprovoked"
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
      filter(Type == input$Type) %>%
      filter(between(Date, input$Date[1], input$Date[2])) %>%
      count(Date, Country) %>%
      ggplot(aes(x = Country)) +
      geom_bar()
     
    
    
  })
}


# Shiny App
shinyApp(ui = ui, server = server)