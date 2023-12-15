# Load common packages and labels ----

source("Code/0- Load common.R")

# Addional packages, for this script only
library("eurostat") # Get statistics from Eurostat API


# Load Eurostat maps ------------------------------------------------------

map_nuts <- read_rds("Data/map_nuts.rds")

# Get data on share of households with broadband access
# https://ec.europa.eu/eurostat/databrowser/product/view/ISOC_R_BROAD_H
nuts_broadband <- get_eurostat("isoc_r_broad_h") %>% 
  # Interested in percentage of total household (as opposed to subset of HH with internet access)
  filter(unit == "PC_HH") %>% 
  mutate(year = year(time)) %>% 
  select(NUTS_ID = geo, year, broadband_shh = values) %>% 
  # Fill missing values (sometimes in the middle of time series) with last observation
  complete(NUTS_ID, year) %>% 
  group_by(NUTS_ID) %>% 
  fill(broadband_shh, .direction = "down") %>% 
  ungroup()

# Show data (and missing values)
nuts_broadband %>% 
  pivot_wider(names_from = year, values_from = broadband_shh) %>% 
  view("Broadband access")

nuts_broadband %>% view()

write_rds(nuts_broadband, "Data/nuts_broadband.rds")

# Plot regional connectivity statistics ----------------------------------------

nuts_broadband <- read_rds("Data/nuts_broadband.rds")

nuts_broadband %>% 
  filter(year >= 2018) %>% 
  left_join(map_nuts, ., by = "NUTS_ID", multiple = "all") %>%
  filter(!is.na(year)) %>% 
  ggplot(aes(fill = broadband_shh)) +
  geom_sf() +
  facet_wrap(~ year, ncol = 2) +
  scale_fill_fermenter("% households", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Internet access is relatively uniform across EU regions",
    subtitle = "Share of households with internet access, by NUTS-1/2 region",
    caption = "Source: Eurostat ICT Household survey, ISOC_R_BROAD_H"
  )

ggsave("Figures/nuts_broadband.png", width = 12, height = 12, units = "cm") # bg = "white")
ggsave("Figures/nuts_broadband.pdf", width = 12, height = 12, units = "cm") # bg = "white")


# Custom connectivity data by NUTS x Degurba ---

speed_2019 <- read_csv("Data/speed_NUTS_DGURBA_2019.csv") %>%
  mutate(
    year = 2019,
    nuts_year = 2016,
  )

speed_2022 <- read_csv("Data/speed_NUTS_DGURBA_2022.csv") %>%
  mutate(
    year = 2022,
    nuts_year = 2021
    )

speed <- bind_rows(speed_2019, speed_2022) %>% 
  mutate(degurba = factor(DGURBA, levels = c(1, 2, 3), labels = c("Cities", "Towns and suburbs", "Rural areas"))) %>% 
  select(-`...1`, - DGURBA)

map_nuts2_degurba <- bind_rows(
    map_nuts %>% filter(LEVL_CODE == 2) %>% mutate(degurba = "Cities"),
    map_nuts %>% filter(LEVL_CODE == 2) %>% mutate(degurba = "Towns and suburbs"),
    map_nuts %>% filter(LEVL_CODE == 2) %>% mutate(degurba = "Rural areas")
  ) %>% 
  mutate(degurba = factor(degurba, levels = c("Cities", "Towns and suburbs", "Rural areas")))


map_nuts2_degurba %>% 
  full_join(speed, by = c("NUTS_ID", "nuts_year", "degurba")) %>% 
  filter(!is.na(degurba)) %>% 
  fill(year) %>% 
  ggplot(aes(fill = avg_speed_mbps )) +
  geom_sf() +
  facet_grid(year ~ degurba) +
  scale_fill_fermenter("Average internet speed\n(Mbps)", palette = "Blues", direction = 1, na.value = "grey80") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  labs(
    title = "Internet speed by region and territorial typology over the years",
    subtitle = "NUTS-2 regions x Degurba territorial typology, 2019 and 2022"
  )

ggsave("Figures/internet_nuts_degurba.png", width = 20, height = 12, units = "cm", bg = "white")
