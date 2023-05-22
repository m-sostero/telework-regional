# Load common packages
source("Code/0- Load packages.R")


# Load LFS, regional telework data, and NUTS maps -------------------------

LFS <- read_feather("Data/LFS.feather") 

regional_telework <- read_dta("Data/regions.dta") %>% mutate(NUTS_ID = reg)

labels_country <- geofacet::eu_grid1 %>% select(country_code = code, country_name = name) %>% as_tibble()

map_nuts <- read_rds("Data/map_nuts.rds")

# Edit EU grid for cartogram to remove UK
eu_grid <- eu_grid1 %>% filter(code != "UK")

# Which NUTS codes are not valid regions either at the NUTS-2 (2021 or 2016) nor NUTS-1, or NUTS-0 level?
regional_telework %>%
  anti_join(map_nuts) %>%
  distinct(NUTS_ID)

# Manually fix some NUTS codes for AT, DE, NL
regional_telework <- regional_telework %>%
  mutate(
    NUTS_ID = reg,
    # Strip last 0 from AT and DE, which are not in the NUTS specs
    NUTS_ID = str_replace_all(NUTS_ID, "(?<=AT\\d)0", ""),
    NUTS_ID = str_replace_all(NUTS_ID, "(?<=DE.)0", ""),
    NUTS_ID = str_replace_all(NUTS_ID, "NL00", "NL")
  )

# After fixing, which NUTS codes are still not valid regions either at the NUTS-2 nor NUTS-1 level?
regional_telework %>%
  anti_join(map_nuts) %>%
  distinct(NUTS_ID)

# Join values for regional telework with NUTS maps
map_regional_telework <- inner_join(map_nuts, regional_telework, by = "NUTS_ID", multiple = "all") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE")) %>%
  mutate_at(c("physicalinteraction", "socialinteraction", "homework_index"), ~ round(., 2))

# Cartogram, faceted by year
map_regional_telework %>%
  filter(!is.na(year)) %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = homework_index)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_c("Homeworking\nindex") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = "Homeworking has increased, in some countries, mostly in capital regions",
    subtitle = "Average values of the homeworking index by region",
    caption = "Homeworking index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/homeworking over time_gradient.pdf", width = 8.27, height = 11.69)

last_plot() +
  scale_fill_viridis_b("Homeworking\nindex")
# scale_fill_fermenter("Homeworking\nindex", palette = "YlGnBu")
ggsave("Figures/homeworking over time_binned.pdf", width = 8.27, height = 11.69)

# Interactive slider map
mapview(
  map_regional_telework %>% filter(year == 2021),
  label = "NAME_LATN", # Label (tooltip) that appears on hover: the Latin name of NUTS region
  zcol = "homework_index", # The variable whose gradient is colored in the cartogram
  legend = FALSE, # No legend for 2019; common legend for 2019-2021
  at = seq(0, 0.6, 0.1), # Defines fill color scale range and increment
  layer.name = "Homeworking 2019"
) |
  mapview(
    map_regional_telework %>% filter(year == 2019),
    label = "NAME_LATN",
    zcol = "homework_index",
    legend = TRUE,
    at = seq(0, 0.6, 0.1),
    layer.name = "Homeworking<br>index<br>(2019 | 2021)"
  )

# Map of changes in homeworking index
regional_telework_change <- regional_telework %>%
  select(year, NUTS_ID, homework_index) %>%
  pivot_wider(names_from = year, values_from = homework_index) %>%
  mutate(delta_hw = `2021` - `2019`) %>%
  select(NUTS_ID, delta_hw)

map_regional_telework_change <- inner_join(map_nuts, regional_telework_change, by = "NUTS_ID", multiple = "all")

scale_limits <- max(abs(regional_telework_change$delta_hw), na.rm = TRUE) * c(-1, 1)

map_regional_telework_change %>%
  st_as_sf() %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = estat_map_nuts0 %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = delta_hw)) +
  scale_fill_fermenter("Change in homeworking index", palette = "RdBu", limit = scale_limits, direction = 1) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(legend.position = "top") +
  labs(
    title = "Homeworking increase is concmostly in capital regions",
    subtitle = "Change in homeworking index 2019–2021 by region",
    caption = "Homeworking index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/homeworking change.pdf", width = 8.27, height = 11.69)
