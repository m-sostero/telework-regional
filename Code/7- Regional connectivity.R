# Load common packages and labels ----

source("Code/0- Load common.R")

# Load map of NUTS regions, prepared in 3-.R
map_nuts <- read_rds("Data/map_nuts.rds")

# JRC connectivity data by NUTS x Degurba ---

# Read all files speed_NUTS_DGURBA_.*.csv and append them in a single table
speed_nuts_degurba <- list.files(path = "Data/", pattern = "speed_NUTS_DGURBA_.*.csv", full.names = T) %>% 
  # Get year from file name, to add as a variable
  set_names(str_extract(., "2...")) %>% 
  # Combine all files in a single table
  map_dfr(
    ~ read_csv(., col_types = cols_only(
      NUTS_ID = "character",
      avg_speed_mbps = "double",
      DGURBA = "character"
      )) %>%
      mutate(degurba = factor(DGURBA, levels = c(1, 2, 3), labels = c("Cities", "Towns and suburbs", "Rural areas")), DGURBA = NULL),
    .id = "year") %>% 
  # Remove observations where degurba = NA. This only happens in Cyprus, which is otherwise complete
  filter(!is.na(degurba)) %>% 
  mutate(year = as.numeric(year))

# Preview the data
speed_nuts_degurba %>%
  # Add NUTS region names
  left_join(labels_nuts, by = "NUTS_ID") %>% 
  relocate(NUTS_name, .after = NUTS_ID) %>%  
  spread(degurba, avg_speed_mbps) %>% 
  arrange(NUTS_ID) %>% 
  view()
  

# Export data of internet speeds by NUTS x Degurba
write_rds(speed_nuts_degurba, "Data/speed_nuts_degurba.rds")

# Plot EU map of internet speeds by NUTS and degurba --- 

speed_nuts_degurba <- read_rds("Data/speed_nuts_degurba.rds")

# Create dummy NUTS Level-2 maps for different degurba
map_nuts2_degurba <- bind_rows(
    map_nuts %>% filter(LEVL_CODE == 2) %>% mutate(degurba = "Cities"),
    map_nuts %>% filter(LEVL_CODE == 2) %>% mutate(degurba = "Towns and suburbs"),
    map_nuts %>% filter(LEVL_CODE == 2) %>% mutate(degurba = "Rural areas")
  ) %>% 
  mutate(degurba = factor(degurba, levels = c("Cities", "Towns and suburbs", "Rural areas")))

# Plot EU map: 
# combine dummy maps NUTS2 x degurba with values of internet speeds
inner_join(map_nuts2_degurba, speed_nuts_degurba, by = c("NUTS_ID", "degurba"), relationship = "many-to-many") %>% 
  filter(!is.na(degurba)) %>% 
  fill(year) %>% 
  ggplot() +
  # Plot all countries in background in grey, (including those not in LFS)
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2019:2023)) %>% sf::st_as_sf(), fill = "grey70") +
  geom_sf(aes(fill = avg_speed_mbps)) +
  # Plot country boundaries (NUTS-0), for all countries
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0), fill = NA, linewidth = 0.5, color = "grey20") +
  facet_grid(year ~ degurba) +
  scale_fill_fermenter("Internet speed\n(Mbps)", palette = "Blues", direction = 1, na.value = "grey70", breaks = c(1, 30, 100)) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme_bw() +
  labs(
    title = "Internet speed by region and territorial typology over the years",
    subtitle = "NUTS-2 regions x Degurba territorial typology, 2019 and 2022"
  ) +
  theme(legend.position = "top")


# Same, but keep only 2019 and 2022
inner_join(map_nuts2_degurba, speed_nuts_degurba, by = c("NUTS_ID", "degurba"), relationship = "many-to-many") %>% 
  filter(!is.na(degurba), ) %>% 
  filter(year %in% c(2019, 2022)) %>% 
  ggplot() +
  # Plot all countries in background in grey, (including those not in LFS)
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2019, 2022)) %>% sf::st_as_sf(), fill = "grey70") +
  geom_sf(aes(fill = avg_speed_mbps)) +
  # Plot country boundaries (NUTS-0), for all countries
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0), fill = NA, linewidth = 0.5, color = "grey20") +
  facet_grid(year ~ degurba) +
  scale_fill_fermenter("Internet speed\n(Mbps)", palette = "Blues", direction = 1, na.value = "grey70", breaks = c(1, 30, 100)) +
  coord_sf(xlim = c(2.6e+6, 6.4e+6), ylim = c(5.5e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme_bw() +
  labs(
    title = "Internet speed by region and degree of urbanisation over the years",
    subtitle = "Average internet speed",
    caption = "Source: Ookla Speedtest Intelligence® data, elaborated by JRC"
  ) +
  theme(legend.position = "top")

ggsave("Figures/internet_nuts_degurba.png", height = 16, width = 20, units = "cm", bg = "white")
ggsave("Figures/internet_nuts_degurba.pdf", height = 16, width = 20, units = "cm", bg = "white")
ggsave("Figures/internet_nuts_degurba.eps", height = 16, width = 20, units = "cm", bg = "white")

# Export images for report

ggsave(path = path_report, filename = "Figure_21_broadband_nuts_degurba.eps", height = 16, width = 20, units = "cm")
ggsave(path = path_report, filename = "Figure_21_broadband_nuts_degurba.png", height = 16, width = 20, units = "cm", bg = "white")
ggsave(path = path_report, filename = "Figure_21_broadband_nuts_degurba.pdf", height = 16, width = 20, units = "cm", bg = "white")
ggsave(path = path_report, filename = "Figure_21_broadband_nuts_degurba.svg", height = 16, width = 20, units = "cm", bg = "white")


# Export as table
speed_nuts_degurba %>% 
  mutate(
    broadband_tier = cut(
      avg_speed_mbps,
      breaks = c(0, 30, 100, Inf),
      labels = c("<30 Mbs", "30-100 Mbs", "> 100 Mbps")
      # labels = 1:3
      )) %>% 
  select(NUTS_ID, degurba, year, broadband_tier) %>%
  filter(year %in% c(2019, 2022)) %>% 
  spread(degurba, broadband_tier) %>% 
  write_xlsx("Tables/NUTS_broadband.xlsx")



# Line plot of speed ranges -----------------------------------------------
speed_nuts_degurba %>% 
  filter(!is.na(degurba)) %>% 
  group_by(year, degurba) %>% 
  summarise(med_reg = mean(avg_speed_mbps, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = med_reg, colour = degurba, group = degurba)) +
  geom_point() + geom_line() +
  geom_dl(aes(label = degurba), method = list(dl.trans(x = x + .3), "last.qp")) +
  scale_x_continuous(limits = c(2019, 2022.5), labels = c(2019:2022, "")) +
  scale_color_brewer(palette = "Set2", direction = -1, guide = NULL) +
  labs(
    title = "Internet speeds have increased across degrees of urbanisation, but the urban-rural gap remains",
    subtitle = "Average internet speed across EU NUTS regions, by degree of urbanisation",
    x = "Year", y = "Internet speed (Mbps)"
  )

# Suppress titles and caption from plot, to include bare graph in Word document
ggplot2::last_plot() + labs(title = NULL, subtitle = NULL, caption = NULL)
ggsave("Figures/Internet_speed_degurba.png", height = 4, width = 7, bg = "white")
ggsave("Figures/Internet_speed_degurba.svg", height = 4, width = 7, bg = "white")
ggsave("Figures/Internet_speed_degurba.pdf", height = 4, width = 7, bg = "white")

speed_nuts_degurba %>% 
  mutate(country = str_sub(NUTS_ID, 1, 2)) %>% 
  filter(!is.na(degurba)) %>% 
  group_by(year, country, degurba) %>% 
  summarise(med_reg = mean(avg_speed_mbps, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = year, y = med_reg, colour = degurba, group = degurba)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name") +
  scale_color_brewer("Degree of urbanisation", palette = "Set2", direction = -1) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = c(0.92, 0.5),
    legend.box.background = element_rect(colour = "grey40")
  ) +
  labs(
    title = "Internet speeds have increased across all degrees of urbanisation, but the urban-rural gap increased slightly",
    subtitle = "Average internet speed across EU NUTS regions, by degree of urbanisation",
    y = "Internet speed (Mbps)"
  )

ggsave("Figures/Internet_speed_degurba_EU.pdf", height = 6, width = 9)
ggsave("Figures/Internet_speed_degurba_EU.png", height = 7, width = 10, bg = "white")
ggsave("Figures/Internet_speed_degurba_EU.svg", height = 7, width = 10, bg = "white")


# Table of speed tiers by region ----------------
speed_nuts_table <- speed_nuts_degurba %>% 
  left_join(labels_nuts, by = "NUTS_ID") %>% 
  mutate(speed_tier = cut(avg_speed_mbps, breaks = c(0,30, 100, Inf), labels = c("Basic", "Fast", "Ultra-fast"))) %>% 
  select(year, NUTS_name, degurba, speed_tier, avg_speed_mbps) 

write_xlsx(speed_nuts_table, "Tables/Internet_speeds_NUTS_degurba.xlsx")


# Deprecated: Eurostat regional connectivity statistics ----

# Addional packages, for this script only
library("eurostat") # Get statistics from Eurostat API

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

nuts_broadband <- read_rds("Data/nuts_broadband.rds")

nuts_broadband %>% 
  filter(year >= 2018) %>% 
  left_join(map_nuts, ., by = "NUTS_ID", multiple = "all") %>%
  filter(!is.na(year)) %>% 
  ggplot(aes(fill = broadband_shh)) +
  geom_sf() +
  # Plot country boundaries (NUTS-0), for all countries
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0), fill = NA, linewidth = 1, color = "grey40") +
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

