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
  rasterVis,
  ggtext,
  grid
)



aus <- ne_countries( country = 'Australia',
                  returnclass = 'sf') %>%
  st_geometry() %>%
  # st_transform('EPSG:3577') %>%
  st_crop(
    xmin = 108,
    xmax = 155,
    ymin = -47,
    ymax = -7
  )


make_hex_grid_aus <- function(cell_size = 2) {
  hexgrid <-
    st_make_grid(aus,
      cellsize = cell_size,
      what = "polygons",
      square = FALSE
    ) %>%
    st_as_sf()

  hexgrid_aus <- hexgrid[aus, ]

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


make_hex_grid_state <- function(cell_size, state_number) {
  aus <- aus[state_number]

  hexgrid <-
    st_make_grid(aus,
      cellsize = cell_size,
      what = "polygons",
      square = FALSE
    ) %>%
    st_as_sf()

  hexgrid_aus <<- hexgrid[aus, ]

  centroids <<-
    hexgrid_aus %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    filter(!between(Y, -41, -39))

  hexgrid_aus$lng <<- centroids$X
  hexgrid_aus$lat <<- centroids$Y
}


land_concord <-
  tibble::tribble(
    ~land_use_descrip, ~catan_use, ~colour,
    "Urban intensive uses", "Ore", "#FF0000",
    "Intensive horticulture and animal production", "Wool", "#FFC9BE",
    "Rural residential and farm infrastructure", "Wool", "#B2B2B2",
    "Plantation forests", "Lumber", "#ADFFB5",
    "Grazing modified pastures", "Wool", "#FFD37F",
    "Mining and waste", "Ore", "#47828F",
    "Dryland cropping", "Grain", "#FFFF00",
    "Dryland horticulture", "Grain", "#AB8778",
    "Water", "Brick", "#0000FF",
    "Nature conservation", "Lumber", "#9666CC",
    "Managed resource protection", "Brick", "#DE87DD",
    "Other minimal use", "Desert", "#C9BEFF",
    "Irrigated pastures", "Wool", "#FFAA00",
    "Irrigated cropping", "Grain", "#C9B854",
    "Irrigated horticulture", "Lumber", "#9C542E",
    "Grazing native vegetation", "Wool", "#FFFFE5",
    "Production native forests", "Lumber", "#298944"
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

# Generate random land use types for 'Managed resource protection', any of Desert, Brick or forest, or Ore
randomise_land_use <- function(df) {
  # Set weights for each land use type
  land_use_types <- c("Desert", "Brick", "Lumber", "Ore")
  weights <- c(0.2, 0.35, 0.1, 0.35)

  df$catan_use <- ifelse(
    df$land_use_descrip == "Managed resource protection",
    sample(land_use_types, sum(df$land_use_descrip == "Managed resource protection"), replace = TRUE, prob = weights),
    df$catan_use
  )

  return(df)
}
