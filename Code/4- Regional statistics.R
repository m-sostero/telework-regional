# Load common packages and labels ----
source("Code/0- Load common.R")

library("broom")

# Load LFS, regional telework data, and NUTS maps -------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") %>% 
  # Remove observations that did not answer to homework question
  filter(!is.na(homework)) %>% 
  # Remove observations with no sampling weights
  filter(!is.na(coeffy)) 

# Maps for NUTS regions, created in '3- Import NUTS maps.R'
map_nuts <- read_rds("Data/map_nuts.rds")

# Retrieve NUTS region names and labels from the maps
labels_nuts <- read_rds("Data/map_nuts.rds") %>% 
  # remove map component, keep unique NUTS names and labels
  st_drop_geometry() %>% as_tibble() %>% 
  select(NUTS_ID, NUTS_name = NAME_LATN) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

# Load occupational teleworking indices
teleworkability <- read_dta("Data/Teleworkability indices.dta") %>%
  mutate(
    isco_3d_code = as.character(isco08_3d),
    physicalinteraction = physicalinteraction/1000
  )


# Compare national values with (summing "sometimes" + "usually")
# https://ec.europa.eu/eurostat/databrowser/bookmark/50019178-d816-4224-a794-b1e7adcd18f8?lang=en

# Compute telework at national level --------------------------------------

# Share teleworking, based on binary indicator homework_any
country_telework <- LFS %>% 
  # Recode urbrur:  "Regions undifferentiated" and missing values as "whole country"
  mutate(urbrur = urbrur %>% fct_recode("Whole country" = "Regions undifferentiated") %>% fct_na_value_to_level(level = "Whole country")) %>% 
  compute_tw_share(year, country)

country_telework %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = year, values_from = telework_share) %>% 
  mutate(change = `2022`/`2019`-1) %>% 
  ggplot(aes(x = `2019`, y = change)) +
  geom_point() + geom_text_repel(aes(label = country)) +
  geom_smooth(method = "loess") +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  # stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),  label.y = -1) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format(prefix = "+")) + 
  labs(
    # title = "Working population working from home 'sometimes' or 'usually'", 
    x = "Share of population working from home (‘sometimes’ or ‘usually’) in 2019",
    y = "Relative growth in work from home 2019-2022",
  )

ggsave("Figures/telework_convergence.png", width = 8, height = 6,  bg = "white")
ggsave("Figures/telework_convergence.svg", width = 8, height = 6,  bg = "white")


LFS %>%
  group_by(year, country, reg) %>%
  mutate(total_group = sum(coeffy, na.rm = TRUE)) %>%  
  group_by(year, country, reg, homework_any) %>%
  summarise(
    n_people = sum(coeffy, na.rm = TRUE),
    pop = mean(total_group),
    .groups = "drop"
  ) %>% 
  filter(homework_any == 1) %>% 
  mutate(telework_share = n_people/pop) %>% 
  group_by(country, year) %>% 
  summarise(
    n_nuts = n(),
    tw_min = min(telework_share, na.rm = T),
    tw_max = max(telework_share, na.rm = T),
    tw_weigh_cv = weighted.mean(telework_share, pop, na.rm = TRUE)/sd(telework_share),
    .groups = "drop"
  ) %>% 
  left_join(country_telework, by = c("year", "country")) %>% 
  rename(tw_country = telework_share) %>% 
  write_xlsx("Tables/Telework_country.xlsx")
  


# Compute telework at regional level --------------------------------------

# Share of people teleworking, based on binary indicator homework_any
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
table_nuts <- regional_telework %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  relocate(country_name, .after = country) %>% 
  left_join(regional_telework_stapro, by = c("year", "reg")) %>% 
  mutate(reglab = str_remove(reglab, "^.{2,4}_")) %>% 
  rename(
    region_NUTS = reg, region_name = reglab, region_type = urbrur,
    `%TW` = telework_share, `%TW (Self-employed)` = `Self-employed`,  `%TW (Employees)` = Employee
  )

table_nuts_change <- regional_telework %>%
  select(year, NUTS_ID = reg, urbrur, telework_share) %>%
  pivot_wider(names_from = year, values_from = telework_share) %>%
  mutate(
    delta_hw_19_21 = (`2021` - `2019`) * 100,
    delta_hw_21_22 = (`2022` - `2021`) * 100,
    delta_hw_19_22 = (`2022` - `2019`) * 100
    ) %>%
  left_join(labels_nuts, by = "NUTS_ID") %>% 
  select(NUTS_ID, NUTS_name, everything())

# Export table Telework by NUTS
write_xlsx(
    list(
      "Telework NUTS stapro" = table_nuts,
      "Telework NUTS change" = table_nuts_change %>% 
        rename( 
          `Diff %TW\n2019-2021 (pp)` = delta_hw_19_21, 
          `Diff %TW\n2021-2022 (pp)` = delta_hw_21_22,
        )
      ),
    "Tables/Telework_NUTS.xlsx"
)

# Ad-hoc table for within-country NUTS variance
table_nuts %>%
  group_by(year, country) %>%
  # Exlude countries with single NUTS
  add_count() %>% filter(n > 1) %>%
  summarise(tw_range = max(`%TW`) - min(`%TW`)) %>%
  group_by(year) %>%
  summarise(mean(tw_range))


# Top teleworking regions --------------------------------

# Compute telework share by intensity by region and year
telework_regions_intensity <- LFS %>%
  group_by(year, reg, reglab, urbrur) %>%
  summarise(
    total_pop = sum(coeffy),
    total_tw_sometimes = sum(coeffy[homework ==  "Person sometimes works at home"]),
    total_tw_usually = sum(coeffy[homework ==  "Person mainly works at home"]),
    Sometimes = total_tw_sometimes/total_pop,
    Usually = total_tw_usually/total_pop,
    tw_any = Sometimes + Usually,
    .groups = "drop"
  ) %>% 
  mutate(reg_type = case_when(
    str_detect(reg, "^LU") ~ "Entire country",
    is.na(urbrur) ~ "Entire country",
    urbrur == "Regions undifferentiated" ~ "Entire country",
    urbrur == "Capital region" ~ "Capital region",
    urbrur != "Capital region" ~ "Other region"
    ) %>% factor(levels = c("Capital region", "Other region", "Entire country"))
    ) %>% 
  select(year, reg, reglab, reg_type, Sometimes, Usually, tw_any) %>% 
  pivot_longer(cols = c(Sometimes, Usually), names_to = "frequency")

# Plot regions with highest rates of telework
telework_regions_intensity %>% 
  filter(year == 2022) %>% 
  slice_max(tw_any, n = 40) %>% 
  arrange(tw_any) %>% 
  # Clean up region labels
  mutate(
    reglab = as.character(reglab) %>% str_replace("_", ": "),
    reglab = str_replace_all(reglab, "^BE10.*", "BE10: Brussels-Capital"),
    reglab = str_wrap(reglab, 30),
    reglab = fct_inorder(reglab), 
  ) %>% 
  ggplot(aes(x = reglab, y = value, fill = reg_type, label = percent(value, accuracy = 0.1, suffix = ""))) +
  geom_col(aes(alpha = frequency, group = frequency), position = "stack") +
  # # Add values inside stacked columns
  # geom_text(position = position_stack(vjust = 0.5), colour = "white") +
  # # Add total values on top of stacked columns
  # geom_text(aes(label = percent(after_stat(y), accuracy  = 0.1, suffix = ""), group = reg_type), stat = 'summary', fun = sum, hjust = 0, colour = "grey50") +
  scale_alpha_manual("Work from home", breaks = c("Usually", "Sometimes"), values = c(1, 0.4)) +
  scale_fill_discrete("Region type", direction = -1, ) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  theme(plot.caption.position = "plot") +
  labs(
    # title = "Capital regions have some of the highest rates of telework",
    # subtitle = "Regions with highest rate of people teleworking in 2021",
    x = "Region", y = "Share of people working from home",
    caption = "Regions are NUTS-2, except for the Netherlands, which is reported at the country level.\n Luxembourg consists of a single NUTS-2 region."
  )

ggsave("Figures/Telework_nuts_top.pdf", height = 5, width = 6)
ggsave("Figures/Telework_nuts_top.png", height = 5, width = 6, bg = "white")
ggsave("Figures/Telework_nuts_top.svg", height = 5, width = 6, bg = "white")
  
  
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
map_regional_telework <- inner_join(map_nuts, regional_telework, by = c("NUTS_ID" = "reg"), multiple = "all", relationship = "many-to-many") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE")) 

# Choropleth, faceted by year
map_regional_telework %>%
  filter(!is.na(year) & year %in% c(2019, 2022)) %>%
  ggplot() +
  # Plot all countries in background in grey, (including those not in LFS)
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2019,2022)) %>% sf::st_as_sf(), color = "white") +
  geom_sf(aes(fill = telework_share), color = "grey70") +
  # Plot country boundaries (NUTS-0), for all countries
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0), fill = NA, linewidth = 0.5, color = "grey50") +
  facet_wrap(. ~ year) +
  scale_fill_viridis_b("Share teleworking", labels = scales::label_percent()) +
  # scale_fill_viridis_c("% Working\nfrom home", labels = scales::label_percent()) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(legend.position = "top", legend.key.width= unit(1, 'cm')) +
  labs(
    # title = "Telework has increased, in some countries, mostly in capital regions",
    # subtitle = "Regional share of the population working from home at least some of the time",
    caption = "Source: EU LFS.\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_nuts_map.pdf", width = 8, height = 3.5)
ggsave("Figures/Telework_nuts_map.png", width = 8, height = 5.5,  bg = "white")
ggsave("Figures/Telework_nuts_map.svg", width = 8, height = 5.5,  bg = "white")


map_regional_telework %>%
  filter(year %in% c(2019, 2022)) %>% 
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2019,2022)) %>% sf::st_as_sf(), fill = "grey70") +
  geom_sf(aes(fill = telework_share)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_b("Share of\nworking\npopulation", labels = scales::label_percent()) +
  # scale_fill_viridis_c("% Working\nfrom home", labels = scales::label_percent()) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme_bw() +
  labs(
    title = "Regional share teleworking",
    subtitle = "Regional share of population working from ‘sometimes’ or ‘usually’",
    caption = "Source: EU LFS.\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Presentation/Telework_nuts_map.png", width = 6, height = 2.5,  bg = "white")
ggsave("Figures/Telework_nuts_map.svg", width = 8, height = 5,  bg = "white")


# Interactive slider map
mapview(
  map_regional_telework %>% filter(year == 2022),
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
    layer.name = "Telework<br>(2019 | 2022)"
  )


# Regional change over time -----

map_regional_telework_change <- table_nuts_change %>% 
  select(NUTS_ID, starts_with("delta")) %>% 
  inner_join(map_nuts, by = "NUTS_ID", multiple = "all") %>% 
  pivot_longer(cols = starts_with("delta"), names_to = "period", values_to = "change") %>% 
  mutate(
    period = str_replace(period, "delta_hw_19_21", "(A) 2019-2021"),
    period = str_replace(period, "delta_hw_21_22", "(B) 2021-2022"),
    period = str_replace(period, "delta_hw_19_22", "(C) 2019-2022")
  )

# compute range of % changes in TW, to center colour scale
scale_limits <- max(abs(c(table_nuts_change$delta_hw_19_21, table_nuts_change$delta_hw_21_22)), na.rm = TRUE) * c(-1, 1)

map_regional_telework_change %>%
  st_as_sf() %>%
  ggplot() +
  # Plot all countries in background in grey, (including those not in LFS)
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2022)) %>% sf::st_as_sf(), fill = "grey70") +
  # Colur NUTS by value (only for countries in LFS)
  geom_sf(aes(fill = change)) +
  # Plot country boundaries (NUTS-0), for all countries
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0), fill = NA, linewidth = 0.6, color = "grey30") +
  facet_wrap(~ period) +
  scale_fill_fermenter("Change (ppts)", palette = "RdBu", limit = scale_limits, direction = 1) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(
    # legend.text.align = 1, 
    legend.position = "top"
    ) +
  labs(
    # title = "Change in regional share of population working from home",
    caption = "Source: LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_nuts_change.pdf", width = 9, height = 4.5)
ggsave("Figures/Telework_nuts_change.png", width = 9, height = 4.5, bg = "white")


map_regional_telework_change %>%
  st_as_sf() %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  geom_sf(aes(fill = delta_hw)) +
  scale_fill_fermenter("Change in share teleworking\n 2019-2021 (pp)", palette = "RdBu", limit = scale_limits, direction = 1) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  theme(legend.position = "top")
ggsave("Presentation/Telework_nuts_change.png", width = 5, height = 6, bg = "white")


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
  mutate(urbrur = urbrur %>% fct_recode("Entire country" = "Regions undifferentiated") %>% fct_na_value_to_level(level = "Entire country")) %>% 
  group_by(year, reg, reglab, urbrur) %>% 
  summarise(
    telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    physicalinteraction = weighted.mean(physicalinteraction, wt = coeffy, na.rm = TRUE),
    pop = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) 

# compute regression telework_share ~ physicalinteraction by year
# Extract Beta of physicalinteraction and R^2 of regression
# Add Beta = and R^2 = as annotation on each facet
reg_year <- regional_teleworkability %>%
  filter(year >= 2019) %>% 
  group_by(year) %>%
  do(results = lm(telework_share ~ physicalinteraction, data = .)) %>%
  ungroup() %>%
  mutate(
    beta = map_dbl(results, ~summary(.x)$coefficients["physicalinteraction", "Estimate"]),
    rsqrd = map_dbl(results, ~summary(.x)$r.squared)*100,
    beta = sprintf("%.2f", beta),
    rsqrd = sprintf("%.1f", rsqrd),
    text = paste0("beta *'='*", beta, " * ';  ' * R^2 * '=' *", rsqrd," * '% ' ")
  )


regional_teleworkability %>%
  filter(year >= 2019) %>% 
  ggplot(aes(x = physicalinteraction, y = telework_share, label = reglab)) +
  geom_point(aes(size = pop, colour = urbrur), shape = 1) +
  geom_smooth(method = lm, mapping = aes(weight = pop)) +
  scale_size_area() +
  facet_grid( ~ year) +
  coord_equal() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    "Type of NUTS region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"),
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Entire country")
  ) +
  guides(size = "none") +
  geom_text(data = reg_year, aes(label = text, x = 0.4, y = 0.6), parse = TRUE, size = 3) +
  labs(
    # title = "Regional uptake of telework ",
    # title = "Correlation between technical teleworkability and actual telework for EU regions",
    x = "Regional mean technical teleworkability", y = "Share working from home"
  ) 

ggsave("Figures/Regional_correlation_teleworkability_telework_urbrur.pdf", height = 3, width = 8)
ggsave("Figures/Regional_correlation_teleworkability_telework_urbrur.png", height = 3, width = 8.5, bg = "white")

ggplot2::last_plot() +
  theme(legend.position = "top")

ggsave("Presentation/Regional_correlation_teleworkability_telework_urbrur.png", height = 4, width = 8, bg = "white")

ggplot2::last_plot() +
  labs(
    title = "Correlation between technical teleworkability and actual telework for EU regions",
    x = "Regional mean technical teleworkability", y = "Share of people working from home"
  ) 

ggsave("Figures/Regional_correlation_teleworkability_telework_urbrur.svg", height = 5, width = 12, bg = "white")

regional_teleworkability %>%
  filter(year %in% c(2019, 2021)) %>% 
  ggplot(aes(x = physicalinteraction, y = telework_share, label = reglab)) +
  geom_point(aes(size = pop, colour = urbrur), shape = 1) +
  geom_smooth(method = lm, mapping = aes(weight = pop)) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size = 3) +
  scale_size_area() +
  facet_grid( ~ year) +
  coord_equal() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    "Type of region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Whole country")
  ) +
  guides(size = "none") +
  labs(
    # title = "Regional uptake of telework ",
    title = "Correlation between technical teleworkability\nand actual telework for EU regions",
    x = "Regional mean technical teleworkability", y = "Share of people teleworking"
  ) 

ggsave("Presentation/Regional_correlation_teleworkability_telework_urbrur_small.png", height = 4, width = 6, bg = "white")

ggsave("Figures/Regional_correlation_teleworkability_telework_urbrur_small.svg", height = 5, width = 7, bg = "white")

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
    subtitle = "Correlation between technical teleworkability and actual telework for EU regions",
    x = "Regional mean technical teleworkability", y = "Share of people teleworking"
  ) 

ggplotly(plot_regional_teleworkability)



# Deprecated --------------------------------------------------------------


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
    direction = "y", hjust = -0.2, na.rm = TRUE, segment.linetype = "dotted", size = 3
  ) +
  scale_shape("Region type") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = "top") +
  scale_x_continuous(limits = c(2017.9, 2021.9), breaks = 2018:2021) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    # title = "The regions with the highest telework rates tend to be capital regions (or surrounding them)",
    # subtitle = "Ten EU NUTS-2 regions with highest share of people teleworking in 2021",
    y = "Share of working population", x = NULL
  )

ggsave("Figures/Telework_nuts_top_2021.pdf", height = 5, width = 8)
ggsave("Figures/Telework_nuts_top_2021.png", height = 5, width = 8, bg = "white")

ggplot2::last_plot() +   labs(
  title = "Capital and urban regions have the highest teleworking rates",
  subtitle = "Ten EU NUTS-2 regions with highest share of people teleworking in 2021",
  y = "Share of working population", x = NULL
)
ggsave("Figures/Telework_nuts_top_2021.svg", height = 5, width = 6.3, bg = "white")


# Lowest teleworking regions ----------------------------------------------

# Same procedure as above
telework_bottom_regions_2021 <- regional_telework %>%
  filter(year == 2021) %>% 
  slice_min(telework_share, n = 10) 

plot_ends <- telework_bottom_regions_2021 %>%
  slice_max(year, n = 1) %>% 
  mutate(
    reglab = as.character(reglab) %>% str_replace("_", ": "),
    reglab = str_wrap(reglab, 30)
  )

regional_telework %>% 
  filter(reg %in% telework_bottom_regions_2021$reg) %>%
  ggplot(aes(x = year, y = telework_share, group = reglab, colour = reglab, shape = urbrur)) +
  geom_point(size = 3) + geom_line() +
  geom_text_repel(
    data = plot_ends, aes(label = reglab, x = year, y = telework_share),
    direction = "y", hjust = -0.2, na.rm = TRUE, segment.linetype = "dotted", size = 3
  ) +
  scale_shape_manual("Region type", values = c(15, 6), limits = c("Intermediate", "Mainly rural")) +
  scale_color_discrete(guide = "none") +
  theme(legend.position = "top") +
  scale_x_continuous(limits = c(2017.9, 2021.9), breaks = 2018:2021) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    # title = "The regions with the lowest telework rates are rural or intermediate",
    subtitle = "Ten EU NUTS-2 regions with lowest share of people teleworking in 2021",
    y = "Share of people teleworking", x = NULL
  )

ggsave("Figures/Telework_nuts_bottom_2021.pdf", height = 5, width = 8)
ggsave("Figures/Telework_nuts_bottom_2021.png", height = 5, width = 8, bg = "white")


ggplot2::last_plot() + labs(
  title = "Rural regions have the lowest teleworking rates",
  subtitle = "Ten EU NUTS-2 regions with lowes share of people teleworking in 2021",
  y = "Share of working population", x = NULL
)

ggsave("Figures/Telework_nuts_bottom_2021.svg", height = 5, width = 6.3, bg = "white")

