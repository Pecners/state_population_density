library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(glue)
library(av)


s <- states() |> 
  st_as_sf()

skip <- c(
  "Puerto Rico",
  # "Alaska",
  "Hawaii",
  "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands",
  "American Samoa",
  "Guam"
)

skinny_s <- s |> 
  filter(!NAME %in% skip) 

skinny_s |> 
  ggplot() +
  geom_sf()

l <- ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(states))

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")

gl <- l %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(skinny_s)) |> 
  st_union()

land <- ne_download(type = "land", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(skinny_s)) |> 
  st_union()


skinny_s <- st_difference(skinny_s, gl)

skinny_s <- st_intersection(skinny_s, land) |> 
  st_transform(crs = 2163)

skinny_s |> 
  ggplot(aes(fill = ifelse(NAME == "Alaska", "#ffb612", "transparent"))) +
  geom_sf(color = "#0f204b") +
  scale_fill_identity() +
  theme_void()

ggsave("assets/alaska/ak_location.svg", bg = "transparent",
       width = 5, h = 5)
