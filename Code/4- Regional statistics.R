# Load common packages and labels ----
source("Code/0- Load common.R")


# Load LFS, regional telework data, and NUTS maps -------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") 

# Maps for NUTS regions
map_nuts <- read_rds("Data/map_nuts.rds")



# Compute telework at regional level --------------------------------------

# Based on binary indicator homework_any
regional_telework <- LFS %>% 
  mutate(homework_any = if_else(homework_index > 0, 1, 0)) %>% 
  # Recode urbrur:  "Regions undifferentiated" and missing values as "whole country"
  mutate(urbrur = urbrur %>% fct_recode("Whole country" = "Regions undifferentiated") %>% fct_na_value_to_level(level = "Whole country")) %>% 
  group_by(year, reg, reglab, urbrur) %>% 
  summarise(homework_any = weighted.mean(homework_index, wt = coeffy, na.rm = TRUE), .groups = "drop")


# Top teleworking regions --------------------------------

telework_top_regions_year <- regional_telework %>%
  group_by(year) %>%
  arrange(year, desc(homework_any)) %>%
  mutate(year_rank = 1:n()) %>%
  group_by(reglab) %>%
  mutate(min_rank = min(year_rank)) %>%
  ungroup()

plot_ends <- telework_top_regions_year %>%
  filter(min_rank <= 5) %>%
  mutate(
    reglab = as.character(reglab),
    reglab = str_replace_all(reglab, "^BE10.*", "BE10_Brussels-Capital"),
    reglab = str_wrap(reglab, 30),
    # Bump some label position manually, to avoid overplotting
    homework_any = if_else(str_detect(reglab, "FI1B.*"), homework_any + 0.005, homework_any),
    homework_any = if_else(str_detect(reglab, "BE31.*"), homework_any - 0.005, homework_any),
    homework_any = if_else(str_detect(reglab, "BE10.*"), homework_any - 0.005, homework_any),
    homework_any = if_else(str_detect(reglab, "FI19.*"), homework_any - 0.005, homework_any)
    ) %>% 
  group_by(reglab) %>%
  top_n(1, year)

telework_top_regions_year %>%
  filter(min_rank <= 5) %>%
  ggplot(aes(x = year, y = homework_any, group = reglab, colour = urbrur)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    sec.axis = sec_axis(~., breaks = plot_ends$homework_any, labels = plot_ends$reglab)
  ) + 
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Whole country")
  ) +
  theme(legend.position = "top") +
  labs(
    title = "The regions with the highest telework rates tend to be capital regions (or surrounding them)",
    subtitle = "Share of population working from home at least some of the time, by region",
    y = "Share of population working from home at least some of the time"
    # caption = 
  )

ggsave("Figures/Telework_nuts_top.pdf", height = 6, width = 9)
ggsave("Figures/Telework_nuts_top.png", height = 6, width = 9, bg = "white")

# Lowest teleworking regions ----------------------------------------------

telework_bottom_regions_year <- regional_telework %>%
  group_by(year) %>%
  arrange(year, homework_any) %>%
  mutate(year_rank = 1:n()) %>%
  group_by(reglab) %>%
  mutate(min_rank = min(year_rank)) %>%
  ungroup()

plot_ends <- telework_bottom_regions_year %>%
  filter(min_rank <= 5) %>%
  mutate(reglab = str_wrap(reglab, 30)) %>% 
  group_by(reglab) %>%
  top_n(1, year)

telework_bottom_regions_year %>%
  filter(min_rank <= 5) %>%
  ggplot(aes(x = year, y = homework_any, group = reglab, colour = urbrur)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    sec.axis = sec_axis(~., breaks = plot_ends$homework_any, labels = plot_ends$reglab)
  ) + 
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Regions undifferentiated")
  ) +
  theme(legend.position = "top") +
  labs(
    title = "The regions with the lowest telework rates are rural and intermediate, in RO, BG, EL",
    subtitle = "Share of population working from home at least some of the time, by region",
    y = "Share of population working from home at least some of the time",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_nuts_bottom.pdf", height = 6, width = 9)
ggsave("Figures/Telework_nuts_bottom.png", height = 6, width = 9, bg = "white")


# Teleworking across all regions ------------------------------------------

plot_telework_urbrur <- regional_telework %>%
  ggplot(aes(x = year, y = homework_any, group = reglab, colour = urbrur)) +
  geom_point() + geom_line(alpha = 0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Regions undifferentiated")
  ) +
  labs(
    title = "Telework has increased most markedly among capital and urban regions",
    subtitle = "Share of population working from home at least some of the time, by region",
    y = "Share of population working from home at least some of the time"
    # caption = 
  )

plot_telework_urbrur

ggplotly(plot_telework_urbrur)


# Regional telework map ---------------------------------------------------

# Ensure that all region names are correct (should return empty table)
anti_join(regional_telework, map_nuts, by = c("reg" = "NUTS_ID"))

# Join values for regional telework with map
map_regional_telework <- inner_join(map_nuts, regional_telework, by = c("NUTS_ID" = "reg"), multiple = "all") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE")) 

#TODO: edit to reflect binary indicator

# Choropleth, faceted by year
map_regional_telework %>%
  filter(!is.na(year)) %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = homework_any)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_c("Homeworking\nindex") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = "Homeworking has increased, in some countries, mostly in capital regions",
    subtitle = "Average values of the homeworking index by region",
    caption = "Homeworking index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("../Figures/homeworking over time_gradient.pdf", width = 8.27, height = 11.69)

last_plot() +
  scale_fill_viridis_b("Homeworking\nindex")
# scale_fill_fermenter("Homeworking\nindex", palette = "YlGnBu")
ggsave("../Figures/homeworking over time_binned.pdf", width = 8.27, height = 11.69)

# Interactive slider map
mapview(
  map_regional_telework %>% filter(year == 2021),
  label = "NAME_LATN", # Label (tooltip) that appears on hover: the Latin name of NUTS region
  zcol = "homework_index", # The variable whose gradient is colored in the cartogram
  legend = FALSE, # No legend for 2019; common legend for 2019-2021
  at = seq(0, 0.6, 0.1), # Defines fill color scale range and increment
  layer.name = "Homeworking 2019"
) |
  mapview(
    map_regional_telework %>% filter(year == 2019),
    label = "NAME_LATN",
    zcol = "homework_index",
    legend = TRUE,
    at = seq(0, 0.6, 0.1),
    layer.name = "Homeworking<br>index<br>(2019 | 2021)"
  )

# Map of changes in homeworking index
regional_telework_change <- regional_telework %>%
  select(year, NUTS_ID, homework_index) %>%
  pivot_wider(names_from = year, values_from = homework_index) %>%
  mutate(delta_hw = `2021` - `2019`) %>%
  select(NUTS_ID, delta_hw)

map_regional_telework_change <- inner_join(map_nuts, regional_telework_change, by = "NUTS_ID", multiple = "all")

scale_limits <- max(abs(regional_telework_change$delta_hw), na.rm = TRUE) * c(-1, 1)

map_regional_telework_change %>%
  st_as_sf() %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = estat_map_nuts0 %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = delta_hw)) +
  scale_fill_fermenter("Change in homeworking index", palette = "RdBu", limit = scale_limits, direction = 1) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(legend.position = "top") +
  labs(
    title = "Homeworking increase is concentrated mostly in capital regions",
    subtitle = "Change in homeworking index 2019–2021 by region",
    caption = "Homeworking index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("../Figures/homeworking change.pdf", width = 8.27, height = 11.69)
