##this app summarizes the total cases in each province in canada as well as in Canada from jan to may

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

# read data
covid_data <- read_csv("covid19_1105.csv")

## Create UI
ui <- fluidPage(
  titlePanel("Canadian Covid cases"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "prname",
        label = "Select origin",
        choices = c("British Columbia", 
                    "Alberta",
                    "Saskatchewan",
                    "Manitoba",
                    "Ontario",
                    "Quebec",
                    "New Brunswick",
                    "Newfoundland and Labrador",
                    "Nova Scotia",
                    "Prince Edward Island",
                    "Northwest Territories",
                    "Nunavut", "Youkon", "Canada"
        )
      ),
      dateRangeInput(
        inputId = "date_range",
        label = "Select date range",
        start = min(covid_data$date),
        end = max(covid_data$date)
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "cases_plot")
    )
  )
)

##Create server
server <- function(input, output) {
  output$cases_plot <- renderPlotly({
    p <- covid_data %>%
      filter(prname == input$prname) %>%
      filter(between(date, input$date_range[1], input$date_range[2])) %>%
      count(date, numtotal) %>%
     ggplot(aes(x = date, y = numtotal)) +
      geom_line(color = "gray") +
      geom_point(aes(color = "orange"), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
      labs(
        title = glue("Daily covid cases in {input$prname} "),
        x = "Date", y = "Cases"
      ) +
      theme_classic() +
      theme(legend.position = "none")
    
   
  })
}
# Shiny App
shinyApp(ui = ui, server = server)
