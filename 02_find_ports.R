## add ports
port_points <-
tribble(~lng,~lat,
       115,-38,
       130,-37,
       120,-17,
       150,-15,
       151,-38,
       140,-42) %>%
  as.data.frame()


find_ports <- function(df){

port_points_sf <- st_as_sf(port_points, coords = c("lng", "lat"), crs = 4326) %>%
  mutate(port = row_number())

find_nearest_hex <- function(value) {

  distances <- st_distance(port_points_sf[value,],

                           hex_grid %>% filter(is_aus == TRUE))
  nearest_hex_index <- which.min(distances)

  nearest <- hex_grid_aus %>% filter(is_aus == TRUE) %>%
    slice(nearest_hex_index)%>%
    dplyr::mutate(port = value) %>%
    dplyr::select(hex_id,port) %>%
    st_drop_geometry()

  direction <-
  bearing(

  hex_grid[nearest_hex_index,] %>%
  st_centroid() %>%
  st_coordinates(),

  port_points_sf[value,] %>%
    st_coordinates()

  )

  nearest$bearing <- direction

  return(nearest)
}


convert_bearing <- function(angle) {
  (angle + 360) %% 360
}

find_nearest_angle <- function(angle, bearings) {
  diffs <- abs(angle - bearings)
  nearest_index <- which.min(diffs)
  return(bearings[nearest_index])
}

# Bearings for the middle of each side of a hexagon
bearings <- seq(30, 330, by = 60)

## how to move between indexes
bearings_df <- data.frame(bearings,
                          hex_id_direction =
                            c(11,20,10,-10,-20,-9))

find_nearest_hex(5)

nearest_ports <- map_dfr(c(1:nrow(port_points_sf)),find_nearest_hex) %>%
  mutate(angle = convert_bearing(bearing))

# Applying the function to each angle
nearest_ports <- nearest_ports %>%
  mutate(nearest_angle = map_dbl(angle, ~ find_nearest_angle(., bearings)))

merged_data <- nearest_ports %>%
  left_join(bearings_df, by = c("nearest_angle" = "bearings")) %>%
  mutate(port_hex_id = hex_id + hex_id_direction)


hex_grid %>%
  ggplot()+
  geom_sf(aes(fill = is_aus))+
  geom_text(aes(x=lng,y=lat,label=hex_id))+
  geom_sf(data = port_points_sf)

map_data %>%
  left_join(nearest_ports) %>%
  mutate(port = replace_na(port,0)) %>%
  ggplot(aes(fill = as.character(port),
             colour = as.character(port)))+
  geom_sf()+
  geom_sf(data = port_points_sf)


df_out <-
df %>%
  mutate(is_port = hex_id %in% merged_data$port_hex_id)


return(df_out)

}
