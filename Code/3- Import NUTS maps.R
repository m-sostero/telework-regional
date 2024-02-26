# Load common packages and labels ----
source("Code/0- Load common.R")

# Download maps in SF format from Eurostat, at NUTS-1,2,3 resolution ----

# Maps are downloaded from Eurostat API, or loaded from local computer cache, if retrieved recently

# Download map at NUTS-0 (country-level)
estat_map_nuts0 <- get_eurostat_geospatial(
  nuts_level = 0,
  year = 2021,
  resolution = "20",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2021)

# Download map at NUTS-1 
estat_map_nuts1 <- get_eurostat_geospatial(
  nuts_level = 1,
  year = 2021,
  resolution = "20",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2021)

# Download map at NUTS-2
estat_map_nuts2 <- get_eurostat_geospatial(
  nuts_level = 2,
  year = 2021,
  resolution = "20",
  crs = "3035",
  output_class = "sf",
  make_valid = TRUE
) %>%
  filter(!CNTR_CODE %in% c("TR")) %>%
  mutate(nuts_year = 2021)

# Combine maps for NUTS 0, 1, 2 in a single table
map_nuts <- bind_rows(estat_map_nuts0, estat_map_nuts1, estat_map_nuts2)

# Export maps for future use
write_rds(map_nuts, "Data/map_nuts.rds")


# Retrieve NUTS codes and names from the maps
labels_nuts <- map_nuts %>% 
  # remove map component, keep unique NUTS names and labels
  st_drop_geometry() %>% as_tibble() %>% 
  select(NUTS_ID, NUTS_name = NAME_LATN) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

# Export NUTS labels
labels_nuts %>% write_csv("Metadata/labels_NUTS.csv")
