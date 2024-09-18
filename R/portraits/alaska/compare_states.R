library(sf)
library(tidyverse)
library(glue)
library(NatParksPalettes)
library(packcircles)
library(patchwork)
library(magick)
library(scales)
library(tigris)
library(treemapify)


# Set map name that will be used in file names, and 
# to get get boundaries from master NPS list

# https://irma.nps.gov/DataStore/Reference/Profile/2225713

data <- st_read("data/nps_boundary/nps_boundary.shp") 

sf_use_s2(FALSE)

# Filter for national parks

parks <- data

st <- states() |> 
  st_transform(crs = st_crs(parks))


areas_by_state <- map_df(1:nrow(parks), function(i) {
  this_p <- parks[i,]
  
  tmp <- map_df(1:nrow(st), function(s) {
    this_s <- st[s,]
    p_area <- st_area(this_p) |> as.numeric()
    
    this_int <- st_intersection(this_p, this_s)
    int_area <- st_area(this_int) |> as.numeric()
    p_perc <- int_area / p_area
    
    tibble(
      park = this_p$PARKNAME,
      state = this_s$NAME,
      park_area_in_state = int_area,
      percent_of_park = p_perc
    )
  })
})

# Combine back with data, calculate summary

state_areas <- areas_by_state |> 
  group_by(state) |> 
  # group_by(is_ak = state == "Alaska") |> 
  summarise(total = sum(park_area_in_state),
            n = n()) |> 
  arrange(desc(total)) |> 
  mutate(perc = label_percent(accuracy = .1)(total / sum(total)))

state_areas |> 
  ggplot(aes(area = total, label = paste0(state, "\n", perc))) +
  geom_treemap(fill = "#0f204b", aes(alpha = total)) +
  geom_treemap_text(place = "center", grow = TRUE,
                    color = "#ffb612", family = "Poller One") +
  scale_alpha_continuous(range = c(.75, 1)) +
  theme(legend.position = "none")

ggsave("assets/alaska/treemap.svg", bg = "transparent",
       width = 5, height = 5)

ggsave("assets/alaska/treemap.png", bg = "transparent",
       width = 5, height = 5)
