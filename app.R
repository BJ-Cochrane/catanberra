library(shiny)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(sf)



# Application title

# Define UI for application
ui <- fluidPage(

  titlePanel("CATANberra"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);",
      # Dropdown menu for Australian states and territories

      # Dynamic drag slider control at the top
      sliderTextInput("size",
                      "Select map size",
                      choices = c("Massive", "Extra large", "Large", "Medium", "Small", "Tiny"),
                      selected = "Medium",
                      animate = TRUE,
                      grid = FALSE,
                      hide_min_max = FALSE,
                      from_fixed = FALSE,
                      to_fixed = FALSE,
                      from_min = NULL,
                      from_max = NULL,
                      to_min = NULL,
                      to_max = NULL,
                      force_edges = FALSE,
                      width = NULL,
                      pre = NULL,
                      post = NULL,
                      dragRange = TRUE),
      checkboxInput("checkbox", "Random?", value = FALSE),
      style = "margin-top: 20px;"
    ),

    # Show the plot
    mainPanel(
      plotOutput("density_plot", width = "100%", height = "800px")
    )
  ),
  # Set background color and font family for the entire UI
  theme = shinytheme("spacelab")
)

# Define server logic
server <- function(input, output, session) {

  plots <- readRDS('plot_list.RDS')

  # Create a named list for easy access
  plots <- setNames(plots, c(1:6))


  random_plot <- readRDS('random_plot_list.RDS')

  # Create a named list for easy access
  random_plot <- setNames(random_plot, c(1:6))


  # Render the plot
  output$density_plot <- renderPlot({

    req(plots[[match(input$size, c("Massive", "Extra large", "Large", "Medium", "Small", "Tiny"))]])

    if(input$checkbox){

      print(random_plot[[match(input$size, c("Massive", "Extra large", "Large", "Medium", "Small", "Tiny"))]])

    } else {
      print(plots[[match(input$size, c("Massive", "Extra large", "Large", "Medium", "Small", "Tiny"))]])
    }

  })
}

# Run the application
shinyApp(ui, server)
