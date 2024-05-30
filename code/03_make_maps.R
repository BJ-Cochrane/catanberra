## make bulk maps
read_land_use(1)
read_land_use(1.5)
read_land_use(2)
read_land_use(2.5)
read_land_use(2.9,-41,-37.5)
read_land_use(5,-41,-37.5)


plot <- .Last.value


save.image('plot.png')

map_sizes <-
tribble(~cell_size,~bass_min,~bass_max,~label,
        1,-41,-37,'Massive',
        1.5,-41,-37,'Extra large',
        2,-41,-37,'Large',
        2.5,-41,-37,'Medium',
        2.9,-41,-37.5,'Small',
        5,-41,-37.5,'Tiny'
        )


save_map <- function(cell_size = 2,
                      bass_min = -41,
                      bass_max = -37,
                      label = 'large',
                      ...){

  chart_lab <- as.character(label)

  plot <-
  read_land_use(cell_size,
                bass_min,
                bass_max)

  return(plot)
}


plot_list <-
list(read_land_use(1),
     read_land_use(1.5),
     read_land_use(2),
     read_land_use(2.5),
     read_land_use(2.9,-41,-37.5),
     read_land_use(5,-41,-37.5)
)


random_plot_list <-
  list(random_map(1),
       random_map(1.5),
       random_map(2),
       random_map(2.5),
       random_map(2.9),
       random_map(5)
  )

saveRDS(plot_list,'plot_list.RDS')
saveRDS(random_plot_list,'random_plot_list.RDS')

save_ggplots <- function(plot_list, file_prefix = "plot", file_format = "png", directory = "./plots/") {
  # Create the directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  # Loop through each plot and save it individually
  for (i in seq_along(plot_list)) {
    filename <- paste0(directory, file_prefix, "_", i, ".", file_format)
    ggsave(filename, plot_list[[i]], width = 655, height = 736,
           units = 'px')  # Adjust width and height as needed
  }
}

save_ggplots(plot_list, "plot", "png", "output/")

