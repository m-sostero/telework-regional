library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("sf") # Simple Features format of maps-as-tables
library("eurostat") # Get data and maps from Eurostat API
library("geofacet") # Arrange plot facets according to (EU) map position

library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps
library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)
theme_set(theme_minimal()) # set minimalist theme as default for ggplot


# Import and process LFS data ---------------------------------------------

# Import, encode and convert LFS
if (!file.exists("../Data/lfs.feather")) {
  read_dta("../Data/LFSreg2018_21finalr.dta") %>%
    # Recode specific values into proper missing values <NA>
    mutate(across(where(is_double), ~ na_if(., 999))) %>%
    mutate(across(where(is_character), ~ na_if(., "99"))) %>%
    mutate(across(where(is_character), ~ na_if(., "999"))) %>%
    as_factor() %>% # Encode categorical variables from STATA as factors in R
    select(-`_merge`) %>%
    write_feather(lfs, "../Data/lfs.feather")
}

LFS <- read_feather("../Data/lfs.feather")


# Maps and NUTS metadata --------------------------------------------------

# EU country names and codes from geofacet library
labels_country <- geofacet::eu_grid1 %>%
  as_tibble() %>%
  select(country_name = name, country_id = code)

# Download maps in SF format from Eurostat, at NUTS-1,2,3 resolution

estat_map_nuts0 <- get_eurostat_geospatial(nuts_level = 0, year = 2021, resolution = "10", crs = "3035", output_class = "sf", make_valid = TRUE) %>%
  filter(!CNTR_CODE %in% c("TR"))

estat_map_nuts1 <- get_eurostat_geospatial(nuts_level = 1, year = 2021, resolution = "10", crs = "3035", output_class = "sf", make_valid = TRUE) %>%
  filter(!CNTR_CODE %in% c("TR"))

estat_map_nuts2 <- get_eurostat_geospatial(nuts_level = 2, year = 2021, resolution = "10", crs = "3035", output_class = "sf", make_valid = TRUE) %>%
  filter(CNTR_CODE != "TR") 

# Croatia split at NUTS-2 level in 2021
estat_map_nuts2_2016 <- get_eurostat_geospatial(nuts_level = 2, year = 2016, resolution = "10", crs = "3035", output_class = "sf", make_valid = TRUE) %>%
  filter(CNTR_CODE != "TR") 


# Regional telework statistics --------------------------------------------

regional_telework <- read_dta("../Data/regions.dta") %>% mutate(NUTS_ID = reg)

# Which NUTS codes are not valid regions either at the NUTS-2 (2021 or 2016) nor NUTS-1, or NUTS-0 level?
regional_telework %>%
  anti_join(estat_map_nuts2) %>%
  anti_join(estat_map_nuts2_2016) %>%
  anti_join(estat_map_nuts1) %>%
  anti_join(estat_map_nuts0) %>%
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
  anti_join(estat_map_nuts2) %>%
  anti_join(estat_map_nuts2_2016) %>%
  anti_join(estat_map_nuts1) %>%
  anti_join(estat_map_nuts0) %>%
  distinct(NUTS_ID)

# Join values for regional telework with map
map_regional_telework <- bind_rows(
    inner_join(estat_map_nuts2, regional_telework, by = "NUTS_ID", multiple = "all"),
    inner_join(estat_map_nuts2_2016, regional_telework, by = "NUTS_ID", multiple = "all"),
    inner_join(estat_map_nuts1, regional_telework, by = "NUTS_ID", multiple = "all"),
    inner_join(estat_map_nuts0, regional_telework, by = "NUTS_ID", multiple = "all")
  ) %>% 
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE")) %>% 
  mutate_at(c("physicalinteraction", "socialinteraction", "homework_index"), ~round(., 2)) 


# Cartogram, faceted by year
map_regional_telework %>% 
  # Add map boundaries for the different years for regions without observations
  # complete(nesting(NUTS_ID, CNTR_CODE, NAME_LATN, geometry), year) %>%
  # filter(!is.na(year)) %>%
  ggplot(aes(fill = homework_index)) +
  geom_sf() +
  facet_wrap(. ~ year) +
  scale_fill_viridis_c("Homeworking\nindex", begin = 0, end = 1, na.value = "grey70") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(legend.position = "top") +
  labs(
    title = "Homeworking has increased, in some countries, mostly in capital regions",
    subtitle = "Average values of the homeworking index by region",
    caption = "Homeworking index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("../Figures/homeworking over time.pdf", width = 8.27, height = 11.69)

# Interactive slider map
  mapview(
    map_regional_telework %>% filter(year == 2021),
    label = "NAME_LATN",
    zcol = "homework_index",
    legend = FALSE,
    at = seq(0, 0.6, 0.1),
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

# Occupational statistics -------------------------------------------------

teleworkability <- read_dta("./Data/Teleworkability indices.dta")

occupational_variables <- read_dta("./Data/occup.dta") %>%
  as_factor() %>%
  arrange(country, year, isco08_3d)

occupational_variables %>%
  filter(country %in% c("DE", "ES", "FR", "IE", "PL", "SE")) %>%
  ggplot(aes(x = physicalinteraction, y = homework_index, size = coeffy)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  scale_size_area() +
  facet_grid(country ~ year) +
  coord_equal() +
  guides(size = "none") +
  labs(
    title = "Telework is reaching its potential",
    subtitle = "Correlation between physical teleworkability and actual telework for ISCO 3-digit occupations",
    x = "Phyisical teleworkability index", y = "Actual telework"
  ) +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


# Regional connectivity statistics ----------------------------------------

# EU regional QOG dataset, contains stats on internet access by region
quog_eureg <- read_csv("./Data/qog_eureg_wide2_nov20.csv")

# IACC is internet access by region
quog_eureg %>% select(region_code, region_name, year, contains("iacc"))

nuts2_internet <- quog_eureg %>%
  select(region_code, region_name, year, eu_is_iacc_nuts2, eu_is_bacc_nuts2) %>%
  filter(year == 2019)

left_join(estat_map_nuts2, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>%
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

ggsave("../Figures/nuts_internet_2019.pdf", width = 12, height = 10, units = "cm", bg = "white")

left_join(estat_map_nuts2, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>%
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

ggsave("../Figures/nuts_broadband_2019.pdf", width = 12, height = 10, units = "cm", bg = "white")

# Interactive map of internet or broadband access by NUTS-2 regions ----

inner_join(estat_map_nuts2, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>% 
  select(NUTS_ID, NAME_LATN, year, eu_is_bacc_nuts2, eu_is_iacc_nuts2, geometry) %>% 
  mapview(
    label = "NAME_LATN",
    zcol = "eu_is_bacc_nuts2",
    legend = TRUE
    )

