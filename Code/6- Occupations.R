# Load common packages and labels ----
source("Code/0- Load common.R")


# Import data -------------------------------------------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") 

# Maps for NUTS regions
map_nuts <- read_rds("Data/map_nuts.rds")

# Occupational teleworking indices
teleworkability <- read_dta("Data/Teleworkability indices.dta") %>%
  mutate(
    isco_3d_code = as.character(isco08_3d),
    physicalinteraction = physicalinteraction/1000
  )


# Telework by country and age ----------------------------------

LFS %>% 
  mutate(year = factor(year)) %>% 
  group_by(year, country, age) %>% 
  summarise(telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = year, y = telework_share, group = age, color = age)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_brewer("Professional status", palette = "Set1") +
  labs(
    title = "Telework has become more common among employees, catching up with the self-employed",
    subtitle = "Share of people teleworking at least some of the time, by professional status",
    y = "Share of people teleworking\n(different scales)"
  )

ggsave("Figures/Telework_stapro_eu.pdf", height = 8, width = 11)
ggsave("Figures/Telework_stapro_eu.png", height = 8, width = 11, bg = "white")


# Table: Telework by country and professional status
table_tw_stapro <- LFS %>% 
  mutate(year = factor(year)) %>% 
  group_by(year, country, stapro) %>% 
  summarise(telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = stapro, names_prefix = "(% TW) ", values_from = telework_share) 

table_tw <- LFS %>% 
  mutate(year = factor(year)) %>% 
  group_by(year, country) %>% 
  summarise(`% TW` = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE), .groups = "drop")

full_join(table_tw, table_tw_stapro, by = c("year", "country")) %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  select(year, country, country_name, everything()) %>% 
  arrange(country, year) %>% 
  write_xlsx("Tables/LFS_telework_stapro.xlsx")


# Telework by country and professional status ----------------------------------

LFS %>% 
  mutate(year = factor(year)) %>% 
  compute_tw_share(year, country, stapro) %>% 
  ggplot(aes(x = year, y = telework_share, group = stapro, color = stapro)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_color_brewer("Professional status", palette = "Set1") +
  labs(
    title = "Telework has become more common among employees, catching up with the self-employed",
    subtitle = "Share of people teleworking at least some of the time, by professional status",
    y = "Share of people teleworking\n(different scales)"
  )

ggsave("Figures/Telework_stapro_eu.pdf", height = 8, width = 11)
ggsave("Figures/Telework_stapro_eu.png", height = 8, width = 11, bg = "white")


# Table: Telework by country and professional status
table_tw_stapro <- LFS %>% 
  compute_tw_share(country, year, stapro) %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = stapro, names_prefix = "(% TW)\n ", values_from = telework_share) %>% 
  left_join(labels_country, by = c("country" = "country_code"))


# Compute EU average 
table_tw_stapro_EU <- LFS %>%
  compute_tw_share(year, stapro) %>% 
  mutate(
    country = "EU-27",
    country_name = "European Union (27)",
  ) %>% 
  select(-n_people) %>% 
  arrange(year, desc(stapro)) %>% 
  pivot_wider(names_from = stapro, names_prefix = "(% TW)\n ", values_from = telework_share) 


# Share of telework by degurba; EU and national figures
bind_rows(
  table_tw_stapro_EU,
  table_tw_stapro
) %>% 
  select(country, country_name, year, everything()) %>% 
  write_xlsx("Tables/Telework_stapro.xlsx")


# Telework by professional status and degurba -----------------------------

# Zoom in on selected countries
selected_countries <- c("DE", "FR", "IT", "ES", "IE", "NL", "SE", "RO")

LFS %>% 
  filter(country %in% selected_countries) %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  mutate(
    year = factor(year),
    stapro = fct_rev(stapro)
    ) %>% 
  compute_tw_share(year, country_name, stapro, degurba) %>% 
  ggplot(aes(x = year, y = telework_share, group = interaction(degurba, stapro), color = degurba, shape = stapro)) +
  geom_point() + geom_line() +
  facet_wrap( ~ country_name, nrow = 2) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  scale_shape_manual("Professional status", values = c(19, 0)) +
  theme(legend.position = "top") +
  labs(
    title = "Telework has become more common among employees, catching up with the self-employed",
    subtitle = "Share of people teleworking at least some of the time, by professional status",
    y = "Share of people teleworking"
  )

ggsave("Figures/Telework_stapro_degurba_selected.pdf", height = 8, width = 11)
ggsave("Figures/Telework_stapro_degurba_selected.png", height = 8, width = 11, bg = "white")



# Regional telework vs teleworkability by professional status ----------------------------------

tw_hw_stapro <- LFS %>%
  left_join(teleworkability, by = "isco_3d_code") %>% 
  group_by(year, country, reglab, stapro) %>% 
  summarise(
    telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    mean_physical_interaction = weighted.mean(physicalinteraction, coeffy, na.rm = TRUE),
    coeffy = sum(coeffy, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) 

reg_year <- tw_hw_stapro %>%
  group_by(year, stapro) %>%
  do(results = lm(telework_share ~ mean_physical_interaction, data = .)) %>%
  ungroup() %>%
  mutate(
    beta = map_dbl(results, ~summary(.x)$coefficients["mean_physical_interaction", "Estimate"]),
    rsqrd = map_dbl(results, ~summary(.x)$r.squared)*100,
    beta = sprintf("%.2f", beta),
    rsqrd = sprintf("%.1f", rsqrd),
    text = paste0("beta *'='*", beta, " * ';  ' * R^2 * '=' *", rsqrd," * '% ' ")
  )
 
tw_hw_stapro %>%  
  ggplot(aes(x = mean_physical_interaction, y = telework_share, group = stapro, color = stapro)) +
  geom_point(aes(size = coeffy), shape = 1, alpha = 0.5) +
  geom_smooth(method = lm) +
  geom_text(data = reg_year %>% filter(stapro == "Self-employed"), aes(label = text, x = 0.3, y = 0.8), parse = TRUE, size = 4, colour = "#E41A1C") + 
  geom_text(data = reg_year %>% filter(stapro == "Employee"),      aes(label = text, x = 0.3, y = 0.7), parse = TRUE, size = 4, colour = "#377EB8") + 
  scale_size_area() +
  facet_grid(~ year) +
  scale_color_brewer("Professional status", palette = "Set1") +
  coord_equal() +
  scale_y_continuous(labels = percent_format()) +
  guides(size = "none") +
  theme(legend.position = "top") + #, panel.spacing = unit(1, "lines")) +
  labs(
    # title = "Telework for employees is catching up with the self-employed",
    # title = "Correlation between technical teleworkability and actual telework for NUTS-2 regions",
    x = "Regional mean technical teleworkability", y = "Share of people working from home"
  )

ggsave("Figures/Regional_correlation_teleworkability_telework_stapro.pdf", height = 5, width = 9)
ggsave("Figures/Regional_correlation_teleworkability_telework_stapro.png", height = 4.5, width = 9, bg = "white")

  
tw_hw_stapro %>%  
  filter(year %in% c(2019, 2021)) %>% 
  ggplot(aes(x = mean_physical_interaction, y = telework_share, group = stapro, color = stapro)) +
  geom_point(aes(size = coeffy), shape = 1, alpha = 0.5) +
  geom_smooth(method = lm) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size = 3) +
  scale_size_area() +
  facet_grid(~ year) +
  scale_color_brewer("Professional status", palette = "Set1") +
  coord_equal() +
  scale_y_continuous(labels = percent_format()) +
  guides(size = "none") +
  # theme(legend.position = "top") + #, panel.spacing = unit(1, "lines")) +
  labs(
    # title = "Telework for employees is cathing up with the self-employed",
    # title = "Correlation between technical teleworkability\nand actual telework for NUTS-2 regions",
    x = "Mean technical teleworkability index", y = "Share of people working from home"
  )

ggplot2::last_plot() +
  labs(subtitle = "Correlation of technical teleworkability and telework for NUTS-2 regions")
ggsave("Presentation/Regional_correlation_teleworkability_telework_stapro_small.png", height = 4, width = 6, bg = "white")
ggsave("Figures/Regional_correlation_teleworkability_telework_stapro.svg", height = 5, width = 7, bg = "white")


# Occupational telework vs teleworkability --------------------------------

tw_hw_isco <- LFS %>%
  inner_join(teleworkability, by = "isco_3d_code") %>% 
  group_by(year, country, isco_3d_code) %>% 
  summarise(
    share_telework = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    mean_physical_interaction = mean(physicalinteraction, na.rm = TRUE),
    coeffy = sum(coeffy, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

# Zoom in on selected countries
selected_countries <- c("DE", "FR", "IT", "ES", "IE", "NL", "SE", "RO")

tw_hw_isco %>%
  filter(country %in% selected_countries) %>%
  ggplot(aes(x = mean_physical_interaction, y = share_telework, size = coeffy)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  scale_size_area() +
  scale_y_continuous(labels = percent_format())+
  facet_grid( year ~ country_name) +
  guides(size = "none") +
  labs(
    title = "Telework is reaching its potential",
    subtitle = "Correlation between technical teleworkability and actual telework for ISCO 3-digit occupations",
    x = "Phyisical teleworkability index", y = "Actual telework"
  ) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave("Figures/Correlation_teleworkability_telework_selected.pdf", height = 6, width = 9)
ggsave("Figures/Correlation_teleworkability_telework_selected.png", height = 6, width = 9, bg = "white")



# Ireland, telework intensity by occupation ----------------------------------------

hw_occupation_freq <- LFS %>%
  group_by(year, country, isco1d, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    homework = fct_recode(
      homework,
      never = "Person never works at home",
      sometimes = "Person sometimes works at home",
      usually = "Person mainly works at home"
    ) %>% fct_rev() 
  ) %>%
  left_join(labels_country, by = c("country" = "country_code"))

hw_occupation_freq %>% 
  filter(country == "IE") %>% 
  filter(!is.na(homework), !isco1d %in% c("Armed forces", "Non response")) %>% 
  mutate(
    year = factor(year),
    isco1d = fct_relabel(isco1d, ~str_wrap(., 30))
    ) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) +
  # geom_col(position = "fill") +
  geom_col(position = "stack") +
  facet_wrap(~ isco1d, ncol = 3) +
  # scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer("Telework", palette = "PuBu") +
  labs(
    title = "In Ireland, professionals teleworking have nearly doubled",
    subtitle = "Telework intensity, by occupation",
    y = "Number of employees (thousands)\nby telework frequency"
  )

ggsave("Figures/Telework_intensity_occup_ireland.pdf", height = 6, width = 9)
ggsave("Figures/Telework_intensity_occup_ireland.png", height = 6, width = 9, bg = "white")


# Variance in telework share by occupation across countries -----------------------

occup_hw_country <- LFS %>%
  # Exclude non-responses, missing, and armed forces
  filter(!isco1d %in% "Non response", !is.na(isco1d), isco1d != "Armed forces") %>%
  # Aggregate at ISCO 2-digit level
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
  # Exclude ISCO 2-digit codes ending in 0 (which are actually 1-digit codes)
  filter(!is.na(isco_2d_code), !str_detect(isco_2d_code, "0$"), ) %>%  
  group_by(year, country, isco_2d_code) %>%
  summarise(telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(labels_isco, by = c("isco_2d_code" = "code")) %>% 
  arrange(isco_2d_code) %>% 
  mutate(
    occupation_label = paste0(isco_2d_code, ": ", occupation),
    occupation_label = factor(occupation_label) %>% fct_inorder() %>% fct_rev()
  )

# Box-plot for ISCO 2-digit occupation, across countries
occup_hw_country %>%
  filter(year == 2021) %>%
  group_by(isco_2d_code, occupation_label, occup_group) %>%
  ggplot(aes(x = occupation_label, y = telework_share, fill = occup_group, label = occupation)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer("Occupation Group\n(ISCO 2008, 1-digit)", palette = "Paired", guide = "none") +
  labs(
    title = "Does the rate of telework vary for the same occupation, across EU countries?",
    subtitle = "Share of population teleworking across countries, by ISCO 2-digit occupation",
    x = "Occupation (ISCO 2008, 2-digit)",
    y = "Share of people teleworking",
    caption = "Source EU-LFS. Employed population, excluding armed forces."
  )

ggsave("Figures/Variation_telework_occupation_boxplot.pdf", height = 6, width = 12)
ggsave("Figures/Variation_telework_occupation_boxplot.png", height = 6, width = 12, bg = "white")


# Teleworking rates for specific occupations ----

occup_homework_freq <- LFS %>%
  # Exclude non-responses, missing, and armed forces
  filter(!isco1d %in% "Non response", !is.na(isco1d), isco1d != "Armed forces") %>%
  # Aggregate at ISCO 2-digit level
  mutate(isco_2d_code = str_sub(isco_3d_code, 1, 2)) %>%
  # Exclude ISCO 2-digit codes ending in 0 (which are actually 1-digit codes)
  filter(!is.na(isco_2d_code), !str_detect(isco_2d_code, "0$")) %>%
  # Sanitise NUTS names %>% 
  rename(NUTS_ID = reg) %>%
  # Manually fix some NUTS codes for AT, DE, NL
  mutate(
    # Strip last 0 from AT and DE, which are not in the NUTS specs
    NUTS_ID = str_replace_all(NUTS_ID, "(?<=AT\\d)0", ""),
    NUTS_ID = str_replace_all(NUTS_ID, "(?<=DE.)0", ""),
    NUTS_ID = str_replace_all(NUTS_ID, "NL00", "NL")
  ) %>% 
  group_by(year, NUTS_ID, isco_2d_code) %>%
  summarise(
    share_telework = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE), 
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco_2d_code" = "code")) %>% 
  arrange(year, isco_2d_code) %>% 
  mutate(
    occupation_label = paste0(isco_2d_code, ": ", occupation),
    occupation_label = factor(occupation_label) %>% fct_inorder() %>% fct_rev()
  )

map_homework_occup_region <- inner_join(map_nuts, occup_homework_freq, by = "NUTS_ID", multiple = "all") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE"))

# Choropleth for ISCO 24
map_homework_occup_region %>%
  filter(isco_2d_code == "24") %>% 
  filter(n_obs > 30) %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = share_telework)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_b("% Teleworking", labels = label_percent()) +
  # scale_fill_viridis_c("% Teleworking", labels = label_percent()) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  # theme(legend.position = "top") +
  labs(
    title = "Rates of telework increasingly vary across EU regions, even for the same occupation",
    subtitle = "Share of people teleworking, by region, for occupation ISCO 24: Business and Administration Professionals",
    caption = "Regions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_isco24_map.pdf", height = 8, width = 9)
ggsave("Figures/Telework_isco24_map.png", height = 8, width = 9, bg = "white")
