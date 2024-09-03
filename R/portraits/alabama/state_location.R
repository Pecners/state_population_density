library(tidyverse)
library(leaflet)
library(mapboxapi)

d <- read_rds("R/portraits/alabama/header.rds")
this_name <- d$map
s <- read_rds("data/states.rds")

this_state <- s |> 
    filter(NAME == str_to_title(str_replace_all(this_name, "_", " ")))

s |> 
    ggplot() +
    geom_sf(fill = NA, color = d$colors[5], linewidth = .2) +
    geom_sf(data = this_state, fill = alpha(d$colors[3], .5),
            color = d$colors[3]) +
    coord_sf(crs = 2163, ylim = c(NA, 700000)) +
    theme_void()

ggsave("assets/alabama/state_location.svg", bg = "transparent",
         width = 10, height = 6)
