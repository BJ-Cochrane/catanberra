pacman::p_load(
  tidyverse,
  sf,
  glue,
  absmapsdata,
  rnaturalearth,
  rnaturalearthdata,
  magick,
  raster,
  tiff,
  terra,
  rmapshaper,
  rasterVis
)

shp <-
read_sf(dsn = "data/aus_land_use/shapefile_currency_clum_50m0917m/currency_clum_50m0917.shp") %>%
  filter(scale == "1:250 000") %>%
  filter(date == max(date))


shp %>%
  slice(2) %>%
  ggplot()+
  geom_sf()

###### Make hex

aus <- ne_countries(scale ='large',
                    country = 'Australia',
                    returnclass = 'sf') %>%
  st_geometry() %>%
  st_transform(4326) %>%
  st_crop(xmin = 108,
          xmax = 155,
          ymin = -47,
          ymax = -7)


make_hex_grid_aus <- function(cell_size = 4){

  hexgrid <-
    st_make_grid(aus,
                 cellsize = cell_size,
                 what = 'polygons',
                 square = FALSE) %>%
    st_as_sf()

  hexgrid_aus <- hexgrid[aus,]

  centroids <<-
    hexgrid_aus %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame()

  hexgrid_aus$lng <- centroids$X
  hexgrid_aus$lat <- centroids$Y

  return(hexgrid_aus)


}


make_hex_grid_state <- function(cell_size,state_number){

  aus <- aus[state_number]

  hexgrid <-
    st_make_grid(aus,
                 cellsize = cell_size,
                 what = 'polygons',
                 square = FALSE) %>%
    st_as_sf()

  hexgrid_aus <<- hexgrid[aus,]

  centroids <<-
    hexgrid_aus %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame()%>%
    filter(!between(Y,-41,-39))

  hexgrid_aus$lng <<- centroids$X
  hexgrid_aus$lat <<- centroids$Y

}


hexgrid_aus <- make_hex_grid_aus(2.5)
# make_hex_grid_state(3,1)




tiles<-
tribble(~icon, ~tile,~fill,~colour,
        "🏺", "clay","#aa6521","#553311",
        "🌵", "desert","#d3ac56","#6a562b",
        "🐑", "sheep","#f7f0d1","#7c7869",
        "🗻", "stone","#a4aaba","#52555d",
        "🌾", "wheat","#97a72a","#4c5415",
        "🌲", "wood","#3e451c","#1f230e")

centroids <-
  centroids %>%
  mutate(tile = sample(c("sheep",
                         "wood",
                         "stone",
                         "clay",
                         "desert",
                         "wheat"), nrow(centroids), replace = TRUE))

centroids <-
  centroids %>%
  filter(!between(Y,-41,-39))


map_data <-
hexgrid_aus %>%
  filter(!between(lat,-41,-39))

map_data$tile <- centroids$tile

  ggplot()+
  geom_sf(data = map_data,
          aes(fill = tile))+
  geom_point(data = centroids,
             aes(X,
                 Y-0.2,
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














