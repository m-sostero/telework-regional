library("tidyverse") # collection of packages for working with data

library("sf") # Simple Features format of maps-as-tables
library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps
theme_set(theme_minimal()) # set minimalist theme as default for ggplot

# Load Eurostat maps
map_nuts <- read_rds("Data/map_nuts.rds")

# Regional connectivity statistics ----------------------------------------

# EU regional QOG dataset, contains stats on internet access by region
quog_eureg <- read_csv("Data/qog_eureg_wide2_nov20.csv")

# IACC is internet access by region
quog_eureg %>% select(region_code, region_name, year, contains("iacc"))

nuts2_internet <- quog_eureg %>%
  select(region_code, region_name, year, eu_is_iacc_nuts2, eu_is_bacc_nuts2) %>%
  filter(year == 2019)

left_join(map_nuts, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>%
  # Apply quantile transformation here mutate()
  ggplot(aes(fill = eu_is_iacc_nuts2)) +
  geom_sf() +
  scale_fill_fermenter("% households", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Share of households with internet access",
    subtitle = "By NUTS-2 region, in 2019",
    caption = "QoG EU Regional dataset, variable eu_is_iacc_nuts2"
  )

ggsave("Figures/nuts_internet_2019.pdf", width = 12, height = 10, units = "cm", bg = "white")

left_join(map_nuts, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>%
  # Apply quantile transformation here mutate()
  ggplot(aes(fill = eu_is_bacc_nuts2)) +
  geom_sf() +
  scale_fill_fermenter("% households", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Share of households with broadband internet access",
    subtitle = "By NUTS-2 region, in 2019",
    caption = "QoG EU Regional dataset, variable eu_is_bacc_nuts2"
  )

ggsave("Figures/nuts_broadband_2019.pdf", width = 12, height = 10, units = "cm", bg = "white")

# Interactive map of internet or broadband access by NUTS-2 regions

inner_join(map_nuts, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>%
  select(NUTS_ID, NAME_LATN, year, eu_is_bacc_nuts2, eu_is_iacc_nuts2, geometry) %>%
  mapview(
    label = "NAME_LATN",
    zcol = "eu_is_bacc_nuts2",
    legend = TRUE
  )
