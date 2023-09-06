# Load common packages used in the various scripts ----

# Silence start-up messages
options(tidyverse.quiet = TRUE) 
suppressMessages({
  library("tidyverse") # collection of packages for working with data

  library("haven") # read/write STATA dta data
  library("arrow") # read/write arrow .feather files

  library("janitor") # convenience functions to clean and adorn summary tables

  library("writexl") # save tables as Excel files

  library("sf") # Simple Features format of maps-as-tables

  library("mapview") # Interactive maps
  library("leafem") # Decorate interactive maps
  library("leaflet.extras2") # Extra utilities to combine interactive maps

  library("geofacet") # Plot facets arranged in approximate EU-shaped map

  library("plotly") # Interative charts

  library("RColorBrewer") # Color gradients for plots
  library("scales") # graph scales (percent, comma, currency)

  theme_set(theme_minimal()) # set minimalist theme as default for ggplot
})

# Define common dictionaries of codes and names for labels ----

# Country codes and names
labels_country <- geofacet::eu_grid1 %>%
  select(country_code = code, country_name = name) %>%
  as_tibble() %>% 
  mutate(country_name = str_replace(country_name, "Czech Republic", "Czechia"))

# Edit EU grid for cartogram to remove UK
eu_grid <- eu_grid1 %>% filter(code != "UK")

# # ISCO-08 occupation labels at 1-4 digits, with corresponding 1-digit group
if (file.exists("Metadata/labels_isco.rds")){
  labels_isco <- read_rds("Metadata/labels_isco.rds")
  # generate from text file, see below)
} else {
  labels_isco <- read_tsv("Metadata/ISCO-08.txt", col_types = "cc") %>%
    mutate(occup_group = if_else(str_length(code) == 1, occupation, NA_character_)) %>%
    fill(occup_group, .direction = "down") %>%
    mutate(occup_group = factor(occup_group) %>% fct_inorder())
  
  write_rds(labels_isco, "Metadata/labels_isco.rds")
}


