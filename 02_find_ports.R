## add ports

port_points <-
tibble::tribble(
                      ~port, ~lat, ~lng,
          "Port of Hedland",   -20,    118.625,
         "Port of Brisbane",  -27.3867,   153.1693,
        "Port of Melbourne",  -37.8175,   144.9672,
        "Port of Fremantle",  -32.0556,   115.7439,
         "Port of Adelaide",  -34.7983,   138.4911,
           "Port of Darwin",  -12.4634,   130.8456,
        "Port of Newcastle",  -32.9265,   151.7804,
       "Port of Townsville",  -19.2564,   146.8233,
        "Port of Gladstone",  -23.8431,   151.2519,
       "Port of Hobart", -42.88333, 147.36667
  )%>%
  as.data.frame()


port_points_sf <- st_as_sf(port_points, coords = c("lng", "lat"), crs = 'WGS84') %>%
  mutate(port = row_number())



find_ports <- function(hex_df){

find_nearest_hex <- function(value = 5) {

  distances <- st_distance(port_points_sf[value,],

                      hex_df %>% filter(is_aus == TRUE) %>%
                        dplyr::select(lng,lat))

  nearest_hex_index <- which.min(distances)

  nearest <- hex_df %>% filter(is_aus == TRUE) %>%
    slice(nearest_hex_index)%>%
    dplyr::mutate(port = value) %>%
    dplyr::select(hex_id,port) %>%
    st_drop_geometry()

  distance_port <- st_distance(
  hex_df %>% filter(hex_id == nearest[1] %>%pull()),
  hex_df %>% filter(is_aus == FALSE)%>%
    dplyr::select(lng,lat)
  )
  nearest_hex_index_port <- which.min(distance_port)

  nearest_port <- hex_df %>% filter(is_aus == FALSE) %>%
    slice(nearest_hex_index_port)


  return(nearest_port)
}


nearest_ports <- map_dfr(c(1:nrow(port_points_sf)),find_nearest_hex)


return(nearest_ports)

}
