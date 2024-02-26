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

  library("plotly") # Interative charts
  library("mapview") # Interactive maps
  library("leafem") # Decorate interactive maps
  library("leaflet.extras2") # Extra utilities to combine interactive maps

  library("geofacet") # Plot facets arranged in approximate EU-shaped map
  library("ggpubr") # Decorate regression plots with coefficients
  library("ggrepel") # Add non-overlapping labels to plot
  library("directlabels") # Label plot lines
  library("RColorBrewer") # Color gradients for plots
  library("scales") # graph scales (percent, comma, currency)
  
  theme_set(theme_minimal()) # set minimalist theme as default for ggplot
})

# Define common dictionaries of codes and names for labels ----

# Country codes and names
labels_country <- geofacet::eu_grid1 %>%
  as_tibble() %>% 
  select(country_code = code, country_name = name) %>%
  as_tibble() %>% 
  mutate(country_name = str_replace(country_name, "Czech Republic", "Czechia"))

# Edit EU grid for cartogram to remove UK
eu_grid <- eu_grid1 %>%
  as_tibble() %>% 
  filter(code != "UK") %>% 
  mutate(name = str_replace(name, "Czech Republic", "Czechia"))

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

# Function to compute share teleworking based on sum of coeffy
# Avoids floating-point problem of weighted.sum(homework_any, coeffy)
compute_tw_share <- function(data, ...){
  grps <- enquos(...)
  
  data %>% 
    group_by(!!!grps) %>%
    mutate(total_group = sum(coeffy, na.rm = TRUE)) %>%  
    group_by(!!!grps, homework_any) %>%
    summarise(
      n_people = sum(coeffy, na.rm = TRUE),
      total_group = mean(total_group),
      .groups = "drop"
    ) %>% 
    filter(homework_any == 1) %>% 
    mutate(telework_share = n_people/total_group) %>% 
    select(-homework_any, -total_group) %>% 
    ungroup() %>% 
    return(.)
}

path_report <- "~/Eurofound/Regional perspective of labour market change during and after COVID-19 - Documents/Drafts/Report/Final figures tables and charts/R"
