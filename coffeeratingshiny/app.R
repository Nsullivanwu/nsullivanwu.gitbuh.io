library(shiny)
library(ggplot2)
library(dplyr)

coffee_data <- tidytuesdayR::tt_load('2020-07-07') |>
  purrr::pluck("coffee_ratings") |>
  filter(!is.na(country_of_origin), !is.na(sweetness)) |> 
  select(total_cup_points, country_of_origin, sweetness) |> 
  group_by(country_of_origin)

ui <- fluidPage(
  titlePanel("Coffee Ratings (by Country) Based on Sweetness Levels"),
  
  selectInput(
    "country", 
    "Select a Country of Origin:",
    choices = coffee_data |>
      dplyr::pull(country_of_origin) |>
      unique(),
    selected = "Ethiopia"
  ),
  
  plotOutput("scatterPlot")
)

server <- function(input, output) {
  new_data <- reactive({
    coffee_data |>
      filter(country_of_origin == input$country)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(new_data(), aes(x = sweetness, y = total_cup_points)) +
      geom_point(color = "black", size = 3, alpha = 0.7) +
      labs(
        title = paste("Coffee Ratings for", input$country),
        x = "Sweetness",
        y = "Total Cup Points"
        ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
