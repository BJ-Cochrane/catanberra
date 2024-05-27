library(shiny)
library(ggplot2)
pacman::p_load(
  tidyverse,
  sf,
  glue,
  # absmapsdata,
  rnaturalearth,
  rnaturalearthdata,
  magick,
  raster,
  tiff,
  terra,
  rmapshaper,
  rasterVis
)


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
                min = 0.1, max = 10, value = 2.5, step = 0.1),
    checkboxInput("checkbox", "Random?", value = FALSE),
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


  land_concord <-
    tibble::tribble(
      ~land_use_descrip, ~catan_use, ~colour,
      "Urban intensive uses", "stone", "#FF0000",
      "Intensive horticulture and animal production", "sheep", "#FFC9BE",
      "Rural residential and farm infrastructure", "sheep", "#B2B2B2",
      "Plantation forests", "wood", "#ADFFB5",
      "Grazing modified pastures", "sheep", "#FFD37F",
      "Mining and waste", "stone", "#47828F",
      "Dryland cropping", "wheat", "#FFFF00",
      "Dryland horticulture", "wheat", "#AB8778",
      "Water", "clay", "#0000FF",
      "Nature conservation", "wood", "#9666CC",
      "Managed resource protection", "clay", "#DE87DD",
      "Other minimal use", "desert", "#C9BEFF",
      "Irrigated pastures", "sheep", "#FFAA00",
      "Irrigated cropping", "wheat", "#C9B854",
      "Irrigated horticulture", "wood", "#9C542E",
      "Grazing native vegetation", "sheep", "#FFFFE5",
      "Production native forests", "wood", "#298944"
    )


  land_ids <-
    read.csv("data/geotiffs/NLUM_INPUTS_250m_2015_16_geo.csv") %>%
    dplyr::select(LUV8N, CL18) %>%
    unique() %>%
    rename(
      "land_use" = LUV8N,
      "land_use_descrip" = CL18
    ) %>%
    left_join(land_concord)

  # Generate random land use types for 'Managed resource protection', any of desert, clay or forest, or stone
  randomise_land_use <- function(df) {
    # Set weights for each land use type
    land_use_types <- c("desert", "clay", "wood", "stone")
    weights <- c(0.4, 0.1, 0.1, 0.4)

    df$catan_use <- ifelse(
      df$land_use_descrip == "Managed resource protection",
      sample(land_use_types, sum(df$land_use_descrip == "Managed resource protection"), replace = TRUE, prob = weights),
      df$catan_use
    )

    return(df)
  }



  ###### Make hex

make_hex_grid_aus <- function(cell_size = 4,geom_input = "WA"){

  aus <- ne_states(country = 'Australia',
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


    hexgrid_aus <-
      hexgrid_aus %>%
      rename("geometry" = x) %>%
      mutate(hex_id = row_number())


    return(hexgrid_aus)


  }


  # Create the plot
  output$density_plot <- renderPlot({

    ## Read tif
    aus_raster <- raster("data/alb/aus_10km_resample.tif", band = 1)

    ## convert to points
    aus_raster_points <-
      rasterToPoints(aus_raster,
                     spatial = TRUE
      )

    hexgrid_aus <- make_hex_grid_aus(input$density, input$state)

    aus_points <- sf::st_as_sf(aus_raster_points, coords = c("x", "y")) %>%
      rename("land_use" = 1) %>%
      left_join(land_ids) %>%
      randomise_land_use() %>%
      st_as_sf() %>%
      st_transform(crs = st_crs(hexgrid_aus))

    joined <-
      aus_points %>%
      st_join(hexgrid_aus, left = TRUE) %>%
      dplyr::select(-lng, -lat)

    ## Get most frequent use by hex
    hex_count <-
      joined %>%
      group_by(hex_id, catan_use) %>%
      add_count() %>%
      group_by(hex_id) %>%
      arrange(-n) %>%
      slice(1) %>%
      dplyr::select(-hex_id)

    catan_map <-
      hexgrid_aus %>%
      st_join(hex_count) %>%
      mutate(catan_use = ifelse(is.na(catan_use), 'wood', catan_use))

    centroids <-
      catan_map %>%
      sf::st_centroid() %>%
      sf::st_coordinates() %>%
      as.data.frame()

    centroids$catan_use <- catan_map$catan_use

    ## remove tasmania
    centroids <-
      centroids %>%
      filter(!between(Y, -41, -39))

    map_data <-
      catan_map %>%
      filter(!between(lat, -41, -39))


    map_data$catan_use <- centroids$catan_use

    raster_colours <-
      land_concord %>%
      filter(land_use_descrip %in% unique(aus_points$land_use_descrip))

    ## Make Catan Map

    aus_points$land_use_descrip <- factor(aus_points$land_use_descrip, levels =raster_colours$land_use_descrip)

    aus_points %>%
      ggplot()+
      geom_sf(aes(colour = land_use_descrip))+
      scale_colour_manual(values = raster_colours$colour)

    ggplot() +
      geom_sf(
        data = map_data,
        aes(fill = catan_use)
      ) +
      geom_point(
        data = centroids,
        aes(X,
            Y - 0.2,
            shape = catan_use,
            colour = catan_use
        ),
        size = 4.5
      ) +
      theme_void() +
      scale_shape_manual(values = tiles$icon) +
      scale_fill_manual(values = tiles$fill) +
      scale_colour_manual(values = tiles$colour) +
      guides(colour = "none") +
      labs(
        fill = "",
        shape = "",
        title = "CATANberra",
        subtitle ='',
        caption = 'Using ABARES 2022, Land use of Australia 2010-11 to 2015-16, 250 m',
      )


    if(input$checkbox){


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
}

  })
}

# Run the application
shinyApp(ui, server)
