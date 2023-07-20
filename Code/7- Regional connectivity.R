library("tidyverse") # collection of packages for working with data

library("sf") # Simple Features format of maps-as-tables
library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps
library("eurostat") # Get statistics from Eurostat API

# Load Eurostat maps
map_nuts <- read_rds("Data/map_nuts.rds")

# Get data on share of households with broadband access
# https://ec.europa.eu/eurostat/databrowser/product/view/ISOC_R_BROAD_H
nuts_broadband <- get_eurostat("isoc_r_broad_h") %>% 
  # Interested in percentage of total household (as opposed to subset of HH with internet access)
  filter(unit == "PC_HH") %>% 
  mutate(year = year(time)) %>% 
  select(NUTS_ID = geo, year, broadband_shh = values) %>% 
  # Fill missing values (sometimes in the middle of time series) with last observation
  complete(NUTS_ID, year) %>% 
  group_by(NUTS_ID) %>% 
  fill(broadband_shh, .direction = "down") %>% 
  ungroup()

# Show data (and missing values)
nuts_broadband %>% 
  pivot_wider(names_from = year, values_from = broadband_shh) %>% 
  view("Broadband access")

nuts_broadband %>% view()

write_rds(nuts_broadband, "Data/nuts_broadband.rds")

# Plot regional connectivity statistics ----------------------------------------

nuts_broadband %>% 
  filter(year >= 2018) %>% 
  left_join(map_nuts, ., by = "NUTS_ID", multiple = "all") %>%
  filter(!is.na(year)) %>% 
  ggplot(aes(fill = broadband_shh)) +
  geom_sf() +
  facet_wrap(~ year, ncol = 2) +
  scale_fill_fermenter("% households", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "At the regional level, the internet access is relatively uniform across EU regions",
    subtitle = "Share of households with internet access, by NUTS-1 or NUTS-2 region",
    caption = "Eurostat ICT Household survey, variable ISOC_R_BROAD_H"
  )

ggsave("Figures/nuts_broadband.pdf", width = 12, height = 10, units = "cm") # bg = "white")
