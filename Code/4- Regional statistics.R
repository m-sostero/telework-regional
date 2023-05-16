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

labels_country <- geofacet::eu_grid1 %>% select(country_code = code, country_name = name) %>% as_tibble()

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
    # coeffy is missing at high rates in some countries. Replacing NA with 1.
    #TODO: check with John
    # weight = replace_na(coeffy , replace = 1),
    homework_index = case_when(
      homework == "Person mainly works at home" ~ 1, 
      homework == "Person sometimes works at home" ~ 0.5, 
      homework == "Person never works at home"  ~ 0 
    )
  ) %>% 
  relocate(homework_index, .after = "homework")


#TODO: why coeffy missing in NL in 2021?
LFS %>%
  group_by(country, year) %>%
  summarise(miss_weights = sum(is.na(coeffy)/n())) %>%
  pivot_wider(values_from = miss_weights, names_from = year) %>%
  mutate(across(`2018`:`2021`, percent)) %>% 
  view()


# Homework index by degurba ----

hw_degurba <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  group_by(year, country, degurba) %>% 
  summarise(homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)), .groups = "drop") %>%
  left_join(labels_country, by = c("country" = "country_code"))

hw_degurba %>% 
  pivot_wider(names_from = degurba, values_from = homework_index) %>% 
  view("Homework by degurba, weighted")


plot_hw_degurba <- hw_degurba %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = country_name, color = country_name)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs(title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_degurba)

hw_degurba %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "Telework has become more common, especially in cities, since 2020",
    subtitle = "Teleworking frequency by degree of urbanisation of respondents' residence, over the years",
    x = "Degree of urbanisation",
    y = "Teleworking frequency index\n(different scales)",
    source = "EU Labour Force Survey,\n own elaboration"
  )

hw_degurba %>% 
  filter(country %in% c("IE", "NL", "IT", "RO")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = year, color = year)) +
  facet_grid(~ country_name) + 
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "Signigicant cross-country differences remain in rates of telework",
    subtitle = "Selected countries",
    x = "Degree of urbanisation",
    y = "Teleworking frequency index",
    source = "EU Labour Force Survey,\n own elaboration"
  )

LFS_employed <- LFS %>%
  group_by(year, country) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop") 

hw_degurba_freq <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  group_by(year, country, degurba, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_employed, by = c("year", "country")) %>% 
  mutate(share = total / total_pop) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

hw_degurba_freq %>% 
  filter(country == "IE", !is.na(homework)) %>% 
  mutate(
    year = factor(year),
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", mainly = "Person mainly works at home") %>% fct_rev()
    ) %>% 
  ggplot(aes(x = homework, y = share, group = year, fill = year)) + geom_col(position = "dodge") +
  facet_wrap(~ degurba) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


hw_degurba_freq %>% 
  filter(country == "IE", !is.na(homework)) %>% 
  mutate(
    year = factor(year),
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", mainly = "Person mainly works at home") 
  ) %>% 
  ggplot(aes(x = year, y = share, group = homework, fill = homework)) + geom_col(position = "stack") +
  facet_wrap(~ degurba) +
  scale_fill_brewer(palette = "PuBu", direction = -1) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


hw_degurba_freq %>% 
  filter(country == "IE", !is.na(homework)) %>% 
  mutate(
    year = factor(year),
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", mainly = "Person mainly works at home") 
  ) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) + geom_col(position = "stack") +
  facet_wrap(~ degurba) +
  scale_fill_brewer("People working\nform home", palette = "PuBu", direction = -1) +
  scale_y_continuous(labels = comma_format(accuracy = 1, scale = 1000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Ireland: Work from home has increased much more than the fall of residents in cities",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Number of people"
  )

hw_degurba_freq %>% 
  filter(country == "NL", !is.na(homework)) %>% 
  mutate(
    year = factor(year),
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", mainly = "Person mainly works at home") 
  ) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) + geom_col(position = "stack") +
  facet_wrap(~ degurba) +
  scale_fill_brewer("People working\nform home", palette = "PuBu", direction = -1) +
  scale_y_continuous(labels = comma_format(accuracy = 1, scale = 1000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Number of people"
  )
  
  

# Homework index by urbrur ----
hw_urbrur <- LFS %>%  
  group_by(year, country, urbrur) %>% 
  summarise(homework_index = weighted.mean(homework_index, w = coeffy, na.rm = TRUE), .groups = "drop") %>% 
  left_join(labels_country, by = c("country" = "country_code"))

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
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

hw_urbrur %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
