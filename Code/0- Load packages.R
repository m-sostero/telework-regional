library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("janitor") # convenience functions to clean and adorn summary tables

library("writexl") # save tables as Excel files

library("sf") # Simple Features format of maps-as-tables

library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps

library("geofacet")

labels_country <- geofacet::eu_grid1 %>%
  select(country_code = code, country_name = name) %>%
  as_tibble()

# Edit EU grid for cartogram to remove UK
eu_grid <- eu_grid1 %>% filter(code != "UK")


library("plotly") # Interative charts

library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)
theme_set(theme_minimal()) # set minimalist theme as default for ggplot


