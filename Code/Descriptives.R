library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("sf") # Simple Features format of maps-as-tables
library("eurostat") # Get data and maps from Eurostat API
library("geofacet") # Arrange plot facets according to (EU) map position

library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)
theme_set(theme_minimal()) # set minimalist theme as default for ggplot


# Import and process LFS data ---------------------------------------------

# Import, encode and convert LFS
if (!file.exists("Data/lfs.feather")) {
  read_dta("./Data/LFSreg2018_21finalr.dta") %>%
    # Recode specific values into proper missing values <NA>
    mutate(across(where(is_double), ~ na_if(., 999))) %>%
    mutate(across(where(is_character), ~ na_if(., "99"))) %>%
    mutate(across(where(is_character), ~ na_if(., "999"))) %>%
    as_factor() %>% # Encode categorical variables from STATA as factors in R
    select(-`_merge`) %>%
    write_feather(lfs, "Data/lfs.feather")
}

LFS <- read_feather("lfs.feather")


# Regional telework statistics --------------------------------------------



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


# Download maps -----------------------------------------------------------

# EU country names and codes from geofacet library
labels_country <- geofacet::eu_grid1 %>%
  as_tibble() %>%
  select(country_name = name, country_id = code)

# Download maps in SF format from Eurostat, at NUTS-2 resolution
estat_map_nuts2 <- get_eurostat_geospatial(nuts_level = 2, year = 2021, resolution = "10", crs = "3035", output_class = "sf", make_valid = TRUE) %>%
  filter(CNTR_CODE != "TR")

estat_map_nuts3 <- get_eurostat_geospatial(nuts_level = 3, year = 2021, resolution = "10", crs = "3035", output_class = "sf", make_valid = TRUE) %>%
  filter(!CNTR_CODE %in% c("TR"))


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
    subtitle = "By NUTS2 region, in 2019",
    caption = "QoG EU Regional dataset, variable eu_is_iacc_nuts2"
  )

ggsave("Figures/nuts_internet_2019.pdf", width = 12, height = 10, units = "cm", bg = "white")

left_join(estat_map_nuts2, nuts2_internet, by = c("NUTS_ID" = "region_code")) %>%
  # Apply quantile transformation here mutate()
  ggplot(aes(fill = eu_is_bacc_nuts2)) +
  geom_sf() +
  scale_fill_fermenter("% households", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Share of households with broadband internet access",
    subtitle = "By NUTS2 region, in 2019",
    caption = "QoG EU Regional dataset, variable eu_is_bacc_nuts2"
  )

ggsave("Figures/nuts_broadband_2019.pdf", width = 12, height = 10, units = "cm", bg = "white")
