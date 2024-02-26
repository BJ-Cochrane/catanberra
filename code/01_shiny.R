library(shiny)
library(ggplot2)
# Application title

# Define UI for application
ui <- fluidPage(

titlePanel("Dynamic Density Control"),

# Sidebar layout with input and output definitions
sidebarLayout(
  sidebarPanel(
    style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);",
    # Dropdown menu for Australian states and territories
    selectInput("state", "Select State/Territory:",
                choices = c("Australia",
                            "ACT",
                            "NSW",
                            "NT",
                            "Qld",
                            "SA",
                            "Tas",
                            "Vic",
                            "WA"),
                selected = "Australia"),

    # Dynamic drag slider control at the top
    sliderInput("density", "Density:",
                min = 1, max = 10, value = 2.5, step = 0.1),
    style = "margin-top: 20px;"
  ),

  # Show the plot
  mainPanel(
    plotOutput("density_plot", width = 1000, height = 800)
  )
)
)


# Define server logic
server <- function(input, output) {

  tiles<-
    tribble(~icon, ~tile,~fill,~colour,
            "🏺", "clay","#aa6521","#553311",
            "🌵", "desert","#d3ac56","#6a562b",
            "🐑", "sheep","#f7f0d1","#7c7869",
            "🗻", "stone","#a4aaba","#52555d",
            "🌾", "wheat","#97a72a","#4c5415",
            "🌲", "wood","#3e451c","#1f230e")


  ###### Make hex

  aus <- ne_states( country = 'Australia',
                      returnclass = 'sf') %>%
    st_geometry() %>%
    st_transform(4326) %>%
    st_crop(xmin = 108,
            xmax = 155,
            ymin = -47,
            ymax = -7)

  aus[1]

  aus[8] %>%
    ggplot()+
    geom_sf()


state_ref <-
  tribble(~code,~state,
          1,"WA",
          2,"NT",
          3,"SA",
          4,"Qld",
          5,"NSW",
          6,"Jervis",
          7, "Vic",
          8, "Tas",
          9, "ACT")

make_hex_grid_aus <- function(cell_size = 4,geom_input = "WA"){

    if(geom_input == "Australia"){
      aus <- ne_countries(scale ='large',
                          country = 'Australia',
                          returnclass = 'sf') %>%
        st_geometry() %>%
        st_transform(4326) %>%
        st_crop(xmin = 108,
                xmax = 155,
                ymin = -47,
                ymax = -7)

    }else{
      code <-
      state_ref %>%
        dplyr::filter(state == geom_input) %>%
        dplyr::select(code) %>%
        pull()
      aus <- aus[code]
    }


    hexgrid <-
      st_make_grid(aus,
                   cellsize = cell_size,
                   what = 'polygons',
                   square = FALSE) %>%
      st_as_sf()

    hexgrid_aus <- hexgrid[aus,]

    centroids <-
      hexgrid_aus %>%
      sf::st_centroid() %>%
      sf::st_coordinates() %>%
      as.data.frame()

    hexgrid_aus$lng <- centroids$X
    hexgrid_aus$lat <- centroids$Y

    return(hexgrid_aus)


  }


  # Create the plot
  output$density_plot <- renderPlot({

    hexgrid_aus <- make_hex_grid_aus(input$density, input$state)

    centroids <- data.frame(hexgrid_aus$lng,hexgrid_aus$lat)
    names(centroids) <- c("lng","lat")

    centroids <-
      centroids %>%
      filter(!between(lat,-41,-39))


    centroids <-
      centroids %>%
      mutate(tile = sample(c("sheep",
                             "wood",
                             "stone",
                             "clay",
                             "desert",
                             "wheat"), nrow(centroids), replace = TRUE))


    map_data <-
      hexgrid_aus %>%
      filter(!between(lat,-41,-39))

    map_data$tile <- centroids$tile

    ggplot()+
      geom_sf(data = map_data,
              aes(fill = tile))+
      geom_point(data = centroids,
                 aes(lng,
                     lat-0.2,
                     shape = tile,
                     colour = tile),
                 size = 4.5) +
      theme_void()+
      scale_shape_manual(values = tiles$icon)+
      scale_fill_manual(values = tiles$fill)+
      scale_colour_manual(values = tiles$colour)+
      guides(colour = "none")+
      labs(fill = '',
           shape = '',
           title = "CATANberra")


  })
}

# Run the application
shinyApp(ui = ui, server = server)
