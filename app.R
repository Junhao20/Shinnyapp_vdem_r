# Upload packages

library(shiny)
library(shinyWidgets)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)

# Load the data

proj_df <- read.csv("proj_data.csv")

# Create list of named values for the input selection

vars <- c(
  "Polyarchy" = "polyarchy",
  "Equal Protection Index" = "equal_protection",
  "Equal Access Index" = "equal_access",
  "Equal Distribution of Resources Index" = "equal_distribution",
  "Civil Society Participation Index" = "civil"
)

# Create list for country names

countries <- proj_df |>
  filter(year == max(year)) |>
  pull(country) |>
  sort()

# ui

ui <- fluidPage(
  titlePanel("Human and gender equality Application"),
  fluidRow(
    column(2, wellPanel(
      selectInput("indicator", "Indicator", vars),
      selectInput("country","Country", countries)
    ),
    helpText("Please click on Country and select the corresponding indicator you would like to see.")
    ), 
    column(6,
           plotOutput("lineChart"),     
           sliderTextInput(
             "range",
             NULL,
             grid = TRUE,
             choices = 1960:2020,
             selected = c(1960, 2020),
             width = "100%"
           )
    )
  )
)

# Server

server <- function(input, output){
  ctry_indicator <- reactive({
    proj_df |>
      filter(country == input$country) |>       # replace INPUT with relevant input
      select(year, input$indicator)     # ditto
  })
  plot_df <- reactive({
    ctry_indicator() |>
      filter(between(year, input$range[1],input$range[2]))   # replace INPUT with relevant input 
  })
  output$lineChart <- renderPlot({
    ggplot(plot_df(), aes(x = year, y = get(input$indicator))) + # replace DATA with appropriate data
      geom_line(color = "navyblue", linewidth = .75) +    # adjust to customize
      labs(
        x = "", 
        y =  names(vars[which(vars == input$indicator)]) +                                    # replace with relevant code
          theme_minimal())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
