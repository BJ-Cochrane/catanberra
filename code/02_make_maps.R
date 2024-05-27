## make bulk maps
read_land_use(1)
 v  read_land_use(1.5)
read_land_use(2)
read_land_use(2.5)
read_land_use(2.9,-41,-37.5)
read_land_use(5,-41,-37.5)


map_sizes <-
tribble(~cell_size,~bass_min,~bass_max,~label,
        1,-41,-37,'Massive',
        1.5,-41,-37,'Extra large',
        2,-41,-37,'Large',
        2.5,-41,-37,'Medium',
        2.9,-41,-37.5,'Small',
        5,-41,-37.5,'Tiny'
        )



save_maps <- function(cell_size,
                      bass_min,
                      bass_max,
                      label,
                      ...){

  chart_lab <- as.character(label)

  read_land_use()





}
