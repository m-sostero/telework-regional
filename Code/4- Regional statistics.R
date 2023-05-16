library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("janitor") # convenience functions to clean and adorn summary tables

library("sf") # Simple Features format of maps-as-tables

library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps

library("geofacet")

library("plotly") # Interative charts

library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)
theme_set(theme_minimal()) # set minimalist theme as default for ggplot


# Load LFS, regional telework data, and NUTS maps -------------------------

LFS <- read_feather("Data/LFS.feather")

regional_telework <- read_dta("Data/regions.dta") %>% mutate(NUTS_ID = reg)

map_nuts <- read_rds("Data/map_nuts.rds")

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


# Compute homework and teleworkability by (year, NUTS, degurba, urbrur ----

LFS <- LFS %>%
  mutate(
    homework_index = case_when(
      homework == "Person mainly works at home" ~ 1, 
      homework == "Person sometimes works at home" ~ 0.5, 
      homework == "Person never works at home"  ~ 0 
    ),
    degurba = factor(degurba, levels = c("Rural areas", "Towns and suburbs", "Cities")),
  ) %>% 
  relocate(homework_index, .after = "homework")

# Homework index by degurba ----
hw_degurba <- LFS %>%  
  group_by(year, country, degurba) %>% 
  summarise(
    homework_index = weighted.mean(homework_index, w = coeffy, na.rm = TRUE)
  ) 

hw_degurba %>% 
  pivot_wider(names_from = degurba, values_from = homework_index) %>% 
  view("Homework by degurba")


plot_hw_degurba <- hw_degurba %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = country, color = country)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs( title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_degurba)

hw_degurba %>% 
  filter(country %in% c("IE", "NL", "IT", "RO")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = year, color = year)) +
  facet_grid(~ country) + 
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

hw_degurba %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



# Homework index by urbrur ----
hw_urbrur <- LFS %>%  
  group_by(year, country, urbrur) %>% 
  summarise(
    homework_index = weighted.mean(homework_index, w = coeffy, na.rm = TRUE)
  ) 

hw_urbrur %>% 
  pivot_wider(names_from = urbrur, values_from = homework_index) %>% 
  view("Homework by urbrur")


plot_hw_urbrur <- hw_urbrur %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = country, color = country)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs( title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_urbrur)

hw_urbrur %>% 
  filter(country %in% c("IE", "DE", "IT", "RO")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  facet_grid(~ country) + 
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

hw_urbrur %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Homework index by urbrur ----
hw_urbrur <- LFS %>%  
  group_by(year, country, urbrur) %>% 
  summarise(
    homework_index = weighted.mean(homework_index, w = coeffy, na.rm = TRUE)
  ) 

hw_urbrur %>% 
  pivot_wider(names_from = urbrur, values_from = homework_index) %>% 
  view("Homework by urbrur")


plot_hw_urbrur <- hw_urbrur %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = country, color = country)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs( title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_urbrur)

hw_urbrur %>% 
  filter(country %in% c("IE", "DE", "IT", "RO")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  facet_grid(~ country) + 
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

hw_urbrur %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))









