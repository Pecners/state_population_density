library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(glue)
library(av)
library(gifski)


s <- states() |> 
  st_as_sf()

skip <- c(
  "Puerto Rico",
  # "Alaska",
  # "Hawaii",
  "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands",
  "American Samoa",
  "Guam"
)


skinny_s <- s |> 
  filter(!NAME %in% skip) 

skinny_s |> 
  as_tibble() |> 
  select(NAME) |> 
  write_csv("data/state_list.csv")


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

skinny_cont <- skinny_s |> 
  filter(!NAME %in% c("Alaska", "Hawaii"))

# skinny_s |> 
#   filter(!NAME %in% c("Alaska", "Hawaii")) |> 
#   ggplot() +
#   geom_sf() +
#   coord_sf(crs = 2163)

# alaska

ak <- skinny_s |> 
  filter(NAME == "Alaska")

shift_one <- function(d, movement_x = .1, movement_y = .1, tilt = 0) {
  
  tmp <- d |> 
    st_union() |> 
    st_centroid() |>
    st_coordinates() |> 
    as_tibble()
  
  bb <- st_bbox(d)
  width <- abs(abs(bb[["xmax"]]) - abs(bb[["xmin"]]))
  height <- abs(abs(bb[["ymax"]]) - abs(bb[["ymin"]]))
  
  # Convert degrees to radians
  rad <- (tilt * pi) / 180
  
  shifted <- d |>
    st_coordinates() |>
    as_tibble() |> 
    mutate(xc = X - tmp[[1,"X"]],
           yc = Y - tmp[[1, "Y"]],
           xf = xc * cos(rad) - yc * sin(rad),
           yf = yc * cos(rad) + xc * sin(rad),
           xx = xf + tmp[[1, "X"]] + width * movement_x,
           yy = yf + tmp[[1, "Y"]] + height * movement_y) |> 
    st_as_sf(coords = c("xx", "yy"), crs = st_crs(d)) |> 
    group_by(L2) |> 
    summarise(do_union = FALSE) |> 
    st_cast(to = "POLYGON") |> 
    rename(geom = geometry)
  
  f <- bind_cols(shifted, population = d$population)
  
  return(f)
}

new <- shift_one(ak, 1.05, -1.45, tilt = 30)

new |> 
    ggplot() +
    geom_sf(data = skinny_cont, color = "white", fill = "#0f204b") +
    geom_sf(color = "#ffb612", fill = alpha("#ffb612", .9)) +
    theme_void()


ggsave("assets/alaska/ak_shifted.png", bg = "transparent",
       width = 5)

ggsave("assets/alaska/ak_shifted.svg", bg = "transparent",
       width = 5)
