library(ggiraph)

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

m_states <- read_rds("data/states.rds")

check_perc <- function(s) {
  t_land <- skinny_s |> 
    as_tibble() |> 
    filter(NAME %in% these_states) |> 
    summarise(total = sum(ALAND)) |> 
    pull(total)
  
  ak_land <- skinny_s |> 
    as_tibble() |> 
    filter(NAME == "Alaska") |> 
    pull(ALAND)
  
  t_land / ak_land
}


these_states <- c(
  "Wisconsin",
  "Illinois",
  "Michigan",
  "Indiana",
  # "Minnesota",
  # "Iowa",
  "Missouri",
  "Kentucky",
  "Tennessee",
  "Mississippi",
  "Alabama",
  "Georgia",
  "Ohio",
  "West Virginia"
  # "North Dakota",
  # "South Dakota",
  # "Kansas",
  # "Nebraska",
  # "Oklahoma"
)

check_perc(these_states)

this_gg <- m_states |> 
  ggplot(aes(fill = ifelse(NAME %in% these_states, TRUE, FALSE))) +
  geom_sf(color = "white") +
  scale_fill_manual(values = c("#0f204b", "#ffb612")) +
  coord_sf(crs = 2163) +
  theme_void() +
  theme(legend.position = "none")

this_gg <- m_states |> 
  ggplot(aes(fill = ifelse(NAME %in% these_states, TRUE, FALSE))) +
  geom_sf_interactive(color = "white", aes(tooltip = NAME)) +
  scale_fill_manual(values = c("#0f204b", "#ffb612")) +
  coord_sf(crs = 2163) +
  theme_void() +
  theme(legend.position = "none")

girafe(this_gg)

ggsave("assets/alaska/states_equal_area.svg", bg = "transparent",
       width = 5)

ggsave("assets/alaska/states_equal_area.png", bg = "transparent",
       width = 5)


# interactive
this_gg <- m_states |> 
  ggplot(aes(fill = ifelse(NAME %in% these_states, TRUE, FALSE))) +
    geom_sf_interactive(color = "white", aes(tooltip = NAME, data_id = NAME)) +
    scale_fill_manual(values = c("#0f204b", "#ffb612")) +
    coord_sf(crs = 2163) +
    theme_void() +
    theme(legend.position = "none")

girafe(ggobj = this_gg, 
       options = list(opts_hover(css = "fill-opacity:.25;stroke:black;stroke-width:0pt;"),
                      opts_tooltip(delay_mouseover = 0)))
