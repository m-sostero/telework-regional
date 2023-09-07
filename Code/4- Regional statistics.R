# Load common packages and labels ----
source("Code/0- Load common.R")


# Load LFS, regional telework data, and NUTS maps -------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") %>% 
  # Remove observations that did not answer to homework question
  filter(!is.na(homework)) %>% 
  # Remove observations with no sampling weights
  filter(!is.na(coeffy)) 

# Maps for NUTS regions
map_nuts <- read_rds("Data/map_nuts.rds")

# NUTS region names
labels_nuts <- read_rds("Data/map_nuts.rds") %>% 
  st_drop_geometry() %>% as_tibble() %>% 
  select(NUTS_ID, NUTS_name = NAME_LATN) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

# Occupational teleworking indices
teleworkability <- read_dta("Data/Teleworkability indices.dta") %>%
  mutate(
    isco_3d_code = as.character(isco08_3d),
    physicalinteraction = physicalinteraction/1000
  )

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
    ungroup() %>% 
    select(!!!grps, telework_share) %>% 
    return(.)
}

# Compare national values with (summing "sometimes" + "usually")
# https://ec.europa.eu/eurostat/databrowser/bookmark/50019178-d816-4224-a794-b1e7adcd18f8?lang=en

# Compute telework at regional level --------------------------------------

# Share teleworking Based on binary indicator homework_any
regional_telework <- LFS %>% 
  # Recode urbrur:  "Regions undifferentiated" and missing values as "whole country"
  mutate(urbrur = urbrur %>% fct_recode("Whole country" = "Regions undifferentiated") %>% fct_na_value_to_level(level = "Whole country")) %>% 
  compute_tw_share(year, country, reg, reglab, urbrur)

# Same, but by stapro
regional_telework_stapro <- LFS %>% 
  compute_tw_share(year, reg, reglab, urbrur, stapro) %>% 
  select(year, reg, stapro, telework_share) %>% 
  pivot_wider(names_from = stapro, values_from = "telework_share")

# Export table of regional telework share
regional_telework %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  relocate(country_name, .after = country) %>% 
  left_join(regional_telework_stapro, by = c("year", "reg")) %>% 
  mutate(reglab = str_remove(reglab, "^.{2,4}_")) %>% 
  rename(
    region_NUTS = reg, region_name = reglab, region_type = urbrur,
    `%TW` = telework_share, `%TW (Self-employed)` = `Self-employed`,  `%TW (Employees)` = Employee
  ) %>% 
  # view("Share teleworking, by NUTS")
  write_xlsx("Tables/Telework_NUTS.xlsx")
  

# Top teleworking regions --------------------------------

# Filter the 10 regions with the highest telework rates in 2021
telework_top_regions_2021 <- regional_telework %>%
  filter(year == 2021) %>% 
  slice_max(telework_share, n = 10) 

# Table with names and values of top 10 regions, to decorate plot. 
plot_ends <- telework_top_regions_2021 %>%
  slice_max(year, n = 1) %>% 
  mutate(
    reglab = as.character(reglab) %>% str_replace("_", ": "),
    reglab = str_replace_all(reglab, "^BE10.*", "BE10: Brussels-Capital"),
    reglab = str_wrap(reglab, 30)
  )

# Retrieve and plot the time series for the 10 regions concerned, add labels at the end
regional_telework %>% 
  filter(reg %in% telework_top_regions_2021$reg) %>%
  ggplot(aes(x = year, y = telework_share, group = reglab, colour = reglab, shape = urbrur)) +
  geom_point(size = 3) + geom_line() +
  geom_text_repel(
    data = plot_ends, aes(label = reglab, x = year, y = telework_share),
    direction = "y", hjust = -0.1, na.rm = TRUE, segment.linetype = "dotted", size = 3
  ) +
  scale_shape("Region type") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0, 0), limits = c(2017.9, 2021.5), breaks = 2018:2021) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    # title = "The regions with the highest telework rates tend to be capital regions (or surrounding them)",
    subtitle = "Ten EU NUTS-2 regions with highest share of people teleworking in 2021",
    y = "Share of people teleworking", x = NULL
  )

ggsave("Figures/Telework_nuts_top_2021.pdf", height = 6, width = 12)
ggsave("Figures/Telework_nuts_top_2021.png", height = 6, width = 12, bg = "white")


# Lowest teleworking regions ----------------------------------------------

# Same procedure as above
telework_bottom_regions_2021 <- regional_telework %>%
  filter(year == 2021) %>% 
  slice_min(telework_share, n = 10) 

plot_ends <- telework_bottom_regions_2021 %>%
  slice_max(year, n = 1) %>% 
  mutate(
    reglab = as.character(reglab) %>% str_replace("_", ": "),
    # Bump some label position manually, to avoid overplotting
    # telework_share = if_else(str_detect(reglab, "BE31.*"), telework_share - 0.005, telework_share),
    reglab = str_wrap(reglab, 30)
  )

regional_telework %>% 
  filter(reg %in% telework_bottom_regions_2021$reg) %>%
  ggplot(aes(x = year, y = telework_share, group = reglab, colour = reglab, shape = urbrur)) +
  geom_point(size = 3) + geom_line() +
  geom_text_repel(
    data = plot_ends, aes(label = reglab, x = year, y = telework_share),
    direction = "y", hjust = -0.3, na.rm = TRUE, segment.linetype = "dotted", size = 3
  ) +
  scale_shape("Region type") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0, 0), limits = c(2017.9, 2021.5), breaks = 2018:2021) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    # title = "The regions with the lowest telework rates are rural or intermediate",
    subtitle = "Ten EU NUTS-2 regions with lowest share of people teleworking in 2021",
    y = "Share of people teleworking", x = NULL
  )

ggsave("Figures/Telework_nuts_bottom_2021.pdf", height = 6, width = 11)
ggsave("Figures/Telework_nuts_bottom_2021.png", height = 6, width = 11, bg = "white")


# Teleworking across all regions ------------------------------------------

plot_telework_urbrur <- regional_telework %>%
  ggplot(aes(x = year, y = telework_share, group = reglab, colour = urbrur)) +
  geom_point() + geom_line(alpha = 0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Whole country")
  ) +
  labs(
    title = "Telework has increased most markedly among capital and urban regions",
    subtitle = "Share of population working from home at least some of the time, by region",
    y = "Share of people teleworking"
  )

plot_telework_urbrur

ggplotly(plot_telework_urbrur)


# Regional telework map ---------------------------------------------------

# Ensure that all region names are correct (should return empty table)
anti_join(regional_telework, map_nuts, by = c("reg" = "NUTS_ID"))

# Join values for regional telework with map
map_regional_telework <- inner_join(map_nuts, regional_telework, by = c("NUTS_ID" = "reg"), multiple = "all") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE")) 

# Choropleth, faceted by year
map_regional_telework %>%
  filter(!is.na(year)) %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  geom_sf(aes(fill = telework_share)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_b("% Teleworking", labels = scales::label_percent()) +
  # scale_fill_viridis_c("% Working\nfrom home", labels = scales::label_percent()) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme_bw() +
  labs(
    # title = "Telework has increased, in some countries, mostly in capital regions",
    subtitle = "Regional share of the population working from home at least some of the time",
    caption = "Source: EU LFS.\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )
ggsave("Figures/Telework_nuts_map.pdf", width = 8, height = 8)
ggsave("Figures/Telework_nuts_map.png", width = 8, height = 8,  bg = "white")


# Interactive slider map
mapview(
  map_regional_telework %>% filter(year == 2021),
  label = "NAME_LATN", # Label (tooltip) that appears on hover: the Latin name of NUTS region
  zcol = "telework_share", # The variable whose gradient is colored in the cartogram
  legend = FALSE, # No legend for 2019; common legend for 2019-2021
  at = seq(0, 0.6, 0.1), # Defines fill color scale range and increment
  layer.name = "Telework 2019"
) |
  mapview(
    map_regional_telework %>% filter(year == 2019),
    label = "NAME_LATN",
    zcol = "telework_share",
    legend = TRUE,
    at = seq(0, 0.6, 0.1),
    layer.name = "Telework<br>(2019 | 2021)"
  )

# Map of changes in share of people teleworking
regional_telework_change <- regional_telework %>%
  select(year, NUTS_ID = reg, telework_share) %>%
  pivot_wider(names_from = year, values_from = telework_share) %>%
  mutate(delta_hw = (`2021` - `2019`) * 100) %>%
  select(NUTS_ID, delta_hw)

map_regional_telework_change <- inner_join(map_nuts, regional_telework_change, by = "NUTS_ID", multiple = "all")

scale_limits <- max(abs(regional_telework_change$delta_hw), na.rm = TRUE) * c(-1, 1)

map_regional_telework_change %>%
  st_as_sf() %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  geom_sf(aes(fill = delta_hw)) +
  scale_fill_fermenter("Change (pp)\n2019-2021", palette = "RdBu", limit = scale_limits, direction = 1) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(legend.text.align = 1) +
  labs(
    # title = "Telework has increased most in capital and urban regions",
    subtitle = "Change in regional share of population working from home, 2019–2021",
    caption = "Source: LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_nuts_change.pdf", width = 8, height = 8)
ggsave("Figures/Telework_nuts_change.png", width = 8, height = 8, bg = "white")


# Telework intensity by country and NUTS ----------------------------------------------

LFS_country_total <- LFS %>%
  group_by(year, country) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop")

hw_country_freq <- LFS %>%
  group_by(year, country, homework) %>% 
  summarise(n_people = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  mutate(degurba = "All") %>% 
  left_join(LFS_country_total, by = c("year", "country")) %>% 
  mutate(share = n_people / total_pop)

LFS_country_degurba_total <- LFS %>%
  group_by(year, country, degurba) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop")

hw_country_degurba_freq <- LFS %>%
  group_by(year, country, degurba, homework) %>% 
  summarise(n_people = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_country_degurba_total, by = c("year", "country", "degurba")) %>% 
  mutate(share = n_people / total_pop) 

hw_country_freq <- bind_rows(hw_country_freq, hw_country_degurba_freq) %>% 
  mutate(homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home")) %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  relocate(country_name, degurba, .after = country) %>% 
  arrange(country, year, degurba, homework)

hw_country_freq_share <- hw_country_freq %>% 
  select(-n_people, -total_pop) %>% 
  pivot_wider(names_from = homework, names_prefix = "% ", values_from = share)

# At NUTS (urbrur) level
LFS_nuts_total <- LFS %>%
  group_by(year, country, reg) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop")

hw_nuts_freq <- LFS %>%
  group_by(year, country, reg, urbrur, homework) %>% 
  summarise(n_people = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_nuts_total, by = c("year", "country", "reg")) %>% 
  mutate(
    share = n_people / total_pop,
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home") %>% fct_rev()
  ) %>% 
  left_join(labels_nuts, by = c("reg" = "NUTS_ID")) %>% 
  select(year, country, NUTS = reg, NUTS_name, everything())

hw_nuts_freq_share <- hw_nuts_freq %>% 
  select(-n_people, -total_pop) %>% 
  pivot_wider(names_from = homework, names_prefix = "% ", values_from = share)

# Export table
write_xlsx(
  list(country_share =  hw_country_freq_share, country_count = hw_country_freq, nuts_share = hw_nuts_freq_share, nuts_count = hw_nuts_freq),
  "Tables/Telework_intensity_country_nuts.xlsx"
  )


# Regional teleworkability vs telework ------------------------------------

# Aggregate actual telework (homework_any) and potential telework (homework_any) by NUTS region
regional_teleworkability <- LFS %>%
  inner_join(teleworkability, by = "isco08_3d") %>%
  # Recode urbrur:  "Regions undifferentiated" and missing values as "whole country"
  mutate(urbrur = urbrur %>% fct_recode("Whole country" = "Regions undifferentiated") %>% fct_na_value_to_level(level = "Whole country")) %>% 
  group_by(year, country, reg, reglab, urbrur) %>% 
  summarise(
    telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    physicalinteraction = weighted.mean(physicalinteraction, wt = coeffy, na.rm = TRUE),
    pop = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) 

year_cor <- regional_teleworkability %>% 
  group_by(year) %>% 
  summarise(cor = cor(telework_share, physicalinteraction)) %>% 
  mutate(year_label = paste0(year, "\n","(R²: ", round(cor, 3),")"))
  

regional_teleworkability %>%
  left_join(year_cor, by = "year") %>% 
  ggplot(aes(x = physicalinteraction, y = telework_share, label = reglab)) +
  geom_point(aes(size = pop, colour = urbrur), shape = 1) +
  geom_smooth(method = lm, mapping = aes(weight = pop)) +
  scale_size_area() +
  facet_grid( ~ year_label) +
  coord_equal() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Whole country")
  ) +
  guides(size = "none") +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(
    title = "Regional uptake of telework ",
    subtitle = "Correlation between physical teleworkability and actual telework for EU regions",
    x = "Regional mean phyisical teleworkability index", y = "Share of people teleworking"
  ) 

ggsave("Figures/Regional_correlation_teleworkability_telework_urbrur.pdf", height = 6, width = 9)
ggsave("Figures/Regional_correlation_teleworkability_telework_urbrur.png", height = 6, width = 9, bg = "white")



# Interactive version
plot_regional_teleworkability <- regional_teleworkability %>%
  filter(year == 2021) %>%
  ggplot(aes(x = physicalinteraction, y = telework_share, label = reglab)) +
  geom_point(aes(size = pop, colour = urbrur), shape = 1) +
  geom_smooth(method = lm) +
  scale_size_area() +
  coord_equal() +
  guides(size = "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Whole country")
  ) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(
    title = "Regional uptake of telework in 2021",
    subtitle = "Correlation between physical teleworkability and actual telework for EU regions",
    x = "Regional mean phyisical teleworkability index", y = "Share of people teleworking"
  ) 

ggplotly(plot_regional_teleworkability)

