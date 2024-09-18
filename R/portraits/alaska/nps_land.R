s <- states() |> 
  st_transform(crs = d_crs)

st <- s |> 
  filter(NAME == "Alaska")

land <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical") |> 
  st_transform(crs = st_crs(data))

st_land <- st_intersection(st, land) |> 
  st_transform(crs = 3338)



parks <- st_read("data/nps_boundary/nps_boundary.shp") |> 
  filter(STATE == "AK") |> 
  st_transform(crs = 3338)

st_land |> 
  ggplot() +
  geom_sf(fill = "#0f204b", color = NA) +
  geom_sf(data = parks, fill = "#ffb612", color = NA) +
  theme_void() +
  coord_sf(crs = 3338, xlim = c(NA, 1500000))

ggsave("assets/alaska/np_bounds.svg", bg = "transparent",
       width = 5, h = 5)

ggsave("assets/alaska/np_bounds.png", bg = "transparent",
       width = 5, h = 5)

ak_land <- st_area(st_union(st_land))
pk_land <- st_area(st_union(parks))

pk_land / ak_land
