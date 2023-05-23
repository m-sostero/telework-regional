# Load common packages and labels ----
source("Code/0- Load packages.R")


# Load LFS, regional telework data, and NUTS maps -------------------------

LFS <- read_feather("Data/LFS.feather") 

map_nuts <- read_rds("Data/map_nuts.rds")



# Telework by region ------------------------------------------------------


# Are NUTS codes from `reg` found in the maps?
LFS %>%
  distinct(reg) %>%
  anti_join(map_nuts, by = c("reg" = "NUTS_ID"))
# (After the fix in 1- Import LFS data.R, the list of non-matching items should be empty)

# compute homework index by region, weighted by coeffy
hw_nuts <- LFS %>% 
  group_by(year, reg, reglab) %>% 
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    .groups = "drop"
  )

# View or export telework index by NUTS
hw_nuts %>%
  spread(year, homework_index) %>% 
  select(reg, reglab, everything()) %>% 
  # write_xlsx("Tables/LFS_telework_index_nuts.xlsx")
  view("Telework index by NUTS")

map_regional_telework <- inner_join(map_nuts, hw_nuts, by = c("NUTS_ID" = "reg"), multiple = "all")

# Choropleth map, faceted by year
map_regional_telework %>%
  filter(!is.na(year)) %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = homework_index)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_b("Telework\nindex") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Telework has increased, in most countries, especially in urban and capital regions",
    subtitle = "Average values of the telework index by NUTS region",
    caption = "Telework index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_nuts_years.pdf", width = 8, height = 8)
ggsave("Figures/Telework_nuts_years.png", width = 8, height = 8, bg = "white")


# Interactive slider map ----
mapview(
  map_regional_telework %>% filter(year == 2021),
  label = "NAME_LATN", # Label (tooltip) that appears on hover: the Latin name of NUTS region
  zcol = "homework_index", # The variable whose gradient is colored in the cartogram
  legend = FALSE, # No legend for 2019; common legend for 2019-2021
  at = seq(0, 0.4, 0.1), # Defines fill color scale range and increment
  layer.name = "Homeworking 2019"
) |
  mapview(
    map_regional_telework %>% filter(year == 2019),
    label = "NAME_LATN",
    zcol = "homework_index",
    legend = TRUE,
    at = seq(0, 0.4, 0.1),
    layer.name = "Telework<br>index<br>(2021 | 2019)"
  )

# Map of changes in homeworking index ----
regional_telework_change <- hw_nuts %>%
  select(year, NUTS_ID = reg, homework_index) %>%
  pivot_wider(names_from = year, values_from = homework_index) %>%
  mutate(delta_hw = `2021` - `2019`) %>%
  select(NUTS_ID, delta_hw)

map_regional_telework_change <- inner_join(map_nuts, regional_telework_change, by = "NUTS_ID", multiple = "first")

scale_limits <- max(abs(regional_telework_change$delta_hw), na.rm = TRUE) * c(-1, 1)

map_regional_telework_change %>%
  st_as_sf() %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = delta_hw)) +
  scale_fill_fermenter("Change in\ntelework index\n(percentage points)", palette = "RdBu", limit = scale_limits, direction = 1) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  # theme(legend.position = "top") +
  labs(
    title = "Telework increased faster in capital and urban regions",
    subtitle = "Change in telework index 2019–2021 by region",
    caption = "Telework index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_nuts_change.pdf", width = 12, height = 8)
ggsave("Figures/Telework_nuts_change.png", width = 12, height = 8, bg = "white")
