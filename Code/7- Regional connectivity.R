library("tidyverse") # collection of packages for working with data

library("sf") # Simple Features format of maps-as-tables
library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps


# Load Eurostat maps
map_nuts <- read_rds("Data/map_nuts.rds")

# Import Eurostat statistics on share of households with internet access by NUTS regions
nuts_broadband <- read_tsv("Data/isoc_r_broad_h_page_tabular.tsv", na = ":") %>% 
  separate(`freq,unit,geo\\TIME_PERIOD`, into = c("freq", "unit", "NUTS_ID"), sep = ",") %>% 
  select(NUTS_ID, `2017`:`2021`) %>% 
  # Reshape in "long" format, to join by NUTS x year later 
  pivot_longer(
    cols = `2017`:`2021`,
    names_to = "year", values_to = "broadband_shh"
    ) %>% 
  mutate(year = as.numeric(year), broadband_shh = as.numeric(broadband_shh)) %>% 
  # Fill missing values during the years with previous values for the same region
  group_by(NUTS_ID) %>% 
  fill(broadband_shh, .direction = "downup") %>% 
  ungroup()

write_rds(nuts_broadband, "Data/nuts_broadband.rds")

# Plot regional connectivity statistics ----------------------------------------

nuts_broadband %>% 
  filter(year >= 2018) %>% 
  inner_join(map_nuts, ., by = "NUTS_ID", relationship = "many-to-many") %>%
  ggplot(aes(fill = broadband_shh)) +
  geom_sf() +
  facet_wrap(~ year, ncol = 2) +
  scale_fill_fermenter("% households", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Share of households with internet access",
    subtitle = "By NUTS-2 region, in 2019",
    caption = "QoG EU Regional dataset, variable isoc_r_broad_h"
  )

ggsave("Figures/nuts_broadband_2019.pdf", width = 12, height = 10, units = "cm") # bg = "white")

