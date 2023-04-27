library("tidyverse") # collection of packages for working with data

library("eurostat") # Get data and maps from Eurostat API
library("geofacet") # Arrange plot facets according to (EU) map position

# EU country names and codes from geofacet library ----
labels_country <- geofacet::eu_grid1 %>%
  as_tibble() %>%
  select(country_name = name, country_id = code)

# Load maps in SF format from Eurostat, at NUTS-1,2,3 resolution ----

# Maps are downloaded from Eurostat API, or loaded from local computer cache, if retrieved recently

estat_map_nuts0 <- get_eurostat_geospatial(
  nuts_level = 0,
  year = 2021,
  resolution = "10",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2021)

estat_map_nuts1 <- get_eurostat_geospatial(
  nuts_level = 1,
  year = 2021,
  resolution = "10",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2021)

estat_map_nuts2 <- get_eurostat_geospatial(
  nuts_level = 2,
  year = 2021,
  resolution = "10",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2021)

# Croatia split at NUTS-2 level in 2021
estat_map_nuts2_2016 <- get_eurostat_geospatial(
  nuts_level = 2,
  year = 2016,
  resolution = "10",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2016)

map_nuts <- bind_rows(estat_map_nuts0, estat_map_nuts1, estat_map_nuts2, estat_map_nuts2_2016)

write_rds(map_nuts, "../Data/map_nuts.rds")
