library(tidyverse)


d <- read_rds("R/portraits/arizona/header.rds")
this_name <- d$map
s <- read_rds("data/states.rds")

this_state <- s |> 
    filter(NAME == str_to_title(str_replace_all(this_name, "_", " ")))

s |> 
    ggplot() +
    geom_sf(fill = NA, color = d$colors[1], linewidth = .2) +
    geom_sf(data = this_state, fill = alpha(d$colors[1], .5),
            color = d$colors[3]) +
    coord_sf(crs = 2163, ylim = c(NA, 700000)) +
    theme_void()

ggsave("assets/arizona/state_location.svg", bg = "transparent",
         width = 10, height = 6)

ggsave("assets/arizona/state_location.png", bg = "white",
         width = 10, height = 6)
