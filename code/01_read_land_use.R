# source("code/00_setup.R")

## Define tiles icons and colours
tiles <-
  tribble(
    ~icon, ~tile, ~fill, ~colour,
    "🏺", "Brick", "#aa6521", "#553311",
    "🌵", "Desert", "#d3ac56", "#6a562b",
    "🌾", "Grain", "#97a72a", "#4c5415",
    "🌲", "Lumber", "#3e451c", "#1f230e",
    "🗻", "Ore", "#a4aaba", "#52555d",
    "🐑", "Wool", "#f7f0d1", "#7c7869"
  )


## Read tif
aus_raster <- raster("data/alb/aus_10km_resample.tif", band = 1)

## convert to points
aus_raster_points <-
  rasterToPoints(aus_raster,
                 spatial = TRUE
  )

set.seed(123)
aus_points <- sf::st_as_sf(aus_raster_points, coords = c("x", "y")) %>%
  rename("land_use" = 1) %>%
  left_join(land_ids) %>%
  randomise_land_use() %>%
  st_as_sf()

read_land_use <- function(cell_size = 2.5,
                          bass_min = -41,
                          bass_max = -39){

hex_grid_aus <- make_hex_grid_aus(cell_size)

ports_hex <- find_ports(hex_grid_aus)
ports_points <- find_ports(hex_grid_aus) %>% st_centroid()

hex_grid_aus <- make_hex_grid_aus(cell_size) %>%
  filter(is_aus == TRUE)

joined <-
  aus_points %>%
  st_transform(crs = st_crs(hex_grid_aus)) %>%
  st_join(hex_grid_aus, left = TRUE) %>%
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
  hex_grid_aus %>%
  st_join(hex_count) %>%
  mutate(catan_use = ifelse(is.na(catan_use), 'Lumber', catan_use))

suppressWarnings(
centroids <-
  catan_map %>%
  sf::st_centroid() %>%
  sf::st_coordinates() %>%
  as.data.frame())

centroids$catan_use <- catan_map$catan_use

## remove bass strait
centroids <-
  centroids %>%
  filter(!between(Y, {bass_min}, {bass_max}))

map_data <-
  catan_map %>%
  filter(!between(lat, {bass_min}, {bass_max}))


map_data$catan_use <- centroids$catan_use

raster_colours <-
land_concord %>%
  filter(land_use_descrip %in% unique(aus_points$land_use_descrip))

## Make Catan Map

aus_points$land_use_descrip <- factor(aus_points$land_use_descrip, levels =raster_colours$land_use_descrip)

catan_colours <- c(
  red = "#A62C2B",   # Deep red
  yellow = "#FFCE00", # Bright yellow
  green = "#8C977D",  # Olive green
  brown = "#B0804C"   # Brown
)

gradient_background <- rasterGrob(
  colorRampPalette(c(catan_colours["red"], catan_colours["yellow"]))(256),
  width = unit(1, "npc"), height = unit(1, "npc")
)

anchor <- '⚓'

map_out <-
ggplot() +
  annotation_custom(gradient_background, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+
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

  # geom_sf(data = ports_hex, alpha = 0.1)+
  geom_sf(data = ports_points, shape = anchor, colour = 'black',  size = 4.5, alpha = 0.4)+


  scale_shape_manual(values = tiles$icon) +
  scale_fill_manual(values = tiles$fill) +
  scale_colour_manual(values = tiles$colour) +
  guides(colour = "none") +
  labs(
    fill = "",
    shape = "",
    title = "<span style='font-size:40pt'><span style='color:#f8ce06;'>**CATAN**</span>berra</span>",
    caption = 'Using ABARES 2022, Satellite data, land use of Australia 2015-16; \n @BenCochraneR',
    x = NULL,
    y = NULL
  )+
  theme_void()+
  theme(plot.title = element_markdown(lineheight = 1.1,colour = 'white'),
        panel.background = element_rect(fill = catan_colours["red"], colour = catan_colours['red']),
        axis.title.x = element_text(size = 15, color = "white"),
        axis.title.y = element_text(size = 15, color = "white"),
        axis.text.x = element_text(size = 12, color =catan_colours['red']),
        axis.text.y = element_text(size = 12, color =catan_colours['red']),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = catan_colours['red'], color = catan_colours["yellow"], size = 3),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.text = element_text(size = 12, color = "white", face = 'bold'), # Increase legend text size
        legend.key.size = unit(30, "cm"),
        legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.byrow = TRUE,
        legend.frame = element_rect(colour = 'black'),
        legend.direction = 'horizontal',
        legend.position = 'top',
        legend.key.spacing.y = unit(-0.05,'cm'),
        plot.margin = unit(c(0.5, 1.5, 0.5, 0.2), "cm"),
        plot.caption = element_text(size = 10, colour = 'white', face = 'italic'),


          )

return(map_out)
}

