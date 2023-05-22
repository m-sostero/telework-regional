# Load common packages
source("Code/0- Load packages.R")


# Occupational teleworkability statistics ---------------------------------------

LFS <- read_feather("Data/LFS.feather") 

teleworkability <- read_dta("Data/Teleworkability indices.dta") %>%
  mutate(
    isco_3d_code = as.character(isco08_3d),
    physicalinteraction = physicalinteraction/1000
  )

occupational_variables <- read_dta("Data/occup.dta") %>%
  as_factor() %>%
  arrange(country, year, isco08_3d)

map_nuts <- read_rds("Data/map_nuts.rds")

labels_isco <- read_rds("Metadata/labels_isco.rds") %>% mutate(code = as.character(code))

tw_hw_degurba <- LFS %>%
  left_join(teleworkability, by = "isco_3d_code") %>% 
  group_by(year, country, reglab, degurba) %>% 
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    teleworkability = (sum(physicalinteraction*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    coeffy = sum(coeffy, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) 

plot_tw_hw_degurba <- tw_hw_degurba %>%  
  ggplot(aes(x = teleworkability, y = homework_index, size = coeffy, label = reglab)) +
  geom_point(aes(color = degurba), shape = 1) +
  geom_smooth(method = lm) +
  scale_size_area() +
  facet_grid(degurba ~ year) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2", guide = "none") +
  coord_equal() +
  guides(size = "none") +
  theme(panel.spacing = unit(1, "lines")) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(
    title = "Increasing correlation between teleworkability and homeworking, similar across location types",
    subtitle = "Correlation between physical teleworkability and actual telework for NUTS-2 regions",
    x = "Phyisical teleworkability index", y = "Homeworking index"
  )

plot_tw_hw_degurba

ggsave("Figures/Correlation_teleworkability_telework_degurba.pdf", height = 6, width = 9)

ggplotly(plot_tw_hw_degurba)
  


# Teleworkability vs telework index ---------------------------------------------


# Zoom in on selected countries
selected_countries <- c("DE", "FR", "IT", "ES", "IE", "NL", "SE", "RO")

occupational_variables %>%
  filter(country %in% selected_countries) %>%
  ggplot(aes(x = physicalinteraction, y = homework_index, size = coeffy)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  scale_size_area() +
  facet_grid( year ~ country) +
  coord_equal() +
  guides(size = "none") +
  labs(
    title = "Telework is reaching its potential",
    subtitle = "Correlation between physical teleworkability and actual telework for ISCO 3-digit occupations",
    x = "Phyisical teleworkability index", y = "Actual telework"
  ) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave("Figures/Correlation_teleworkability_telework_selected.pdf", height = 6, width = 9)


tw_hw_degurba <- LFS %>%
  group_by(year, country, isco_3d_code, degurba) %>% 
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    coeffy = sum(coeffy, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  left_join(teleworkability, by = "isco_3d_code") 

tw_hw_degurba %>%
  filter(country %in% c("IE")) %>% 
  ggplot(aes(x = physicalinteraction, y = homework_index,  group = degurba, color = degurba, size = coeffy)) + 
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2", direction = -1, guide = "none") +
  scale_size_area() +
  facet_grid(degurba ~ year) +
  coord_equal() +
  guides(size = "none") +
  labs(
    title = "Telework is reaching its potential",
    subtitle = "Correlation between physical teleworkability and actual telework for ISCO 3-digit occupations",
    x = "Phyisical teleworkability index", y = "Actual telework"
  ) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))



# Telework frequency by occupation ----------------------------------------

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

# Ireland, share of employees by occupation
hw_occupation_freq %>% 
  filter(country == "IE") %>% 
  filter(!is.na(homework), !isco1d %in% c("Armed forces", "Non response")) %>% 
  mutate(
    year = factor(year),
    isco1d = fct_relabel(isco1d, ~str_wrap(., 30))
    ) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) +
  geom_col(position = "fill") +
  facet_wrap(~ isco1d, ncol = 3) +
  scale_fill_brewer("Work at home", palette = "PuBu") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Ireland",
    subtitle = "Teleworkability intensity, by occupation",
    y = "Share of employees, by telework frequency"
  )

ggsave("Figures/Telework_intensity_occup_ireland.pdf", height = 6, width = 9)


# Telework index by occupation, year, country -----------------------

occup_homework_country <- LFS %>%
  # Exclude non-responses, missing, and armed forces
  filter(!isco1d %in% "Non response", !is.na(isco1d), isco1d != "Armed forces") %>%
  # Aggregate at ISCO 2-digit level
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
  # Exclude ISCO 2-digit codes ending in 0 (which are actually 1-digit codes)
  filter(!is.na(isco_2d_code), !str_detect(isco_2d_code, "0$"), ) %>%  
  group_by(year, country, isco_2d_code) %>%
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco_2d_code" = "code")) %>% 
  arrange(isco_2d_code) %>% 
  mutate(
    occupation_label = paste0(isco_2d_code, ": ", occupation),
    occupation_label = factor(occupation_label) %>% fct_inorder() %>% fct_rev()
  )

# Box-plot for ISCO 2-digit occupation, across countries
occup_homework_country %>%
  filter(year == 2021) %>%
  group_by(isco_2d_code, occupation_label, occup_group) %>%
  ggplot(aes(x = occupation_label, y = homework_index, fill = occup_group, label = occupation)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer("Occupation Group\n(ISCO 2008, 1-digit)", palette = "Paired", guide = "none") +
  labs(
    title = "Does the rate of telework vary for the same occupation, across EU countries?",
    subtitle = "Distribution of telework index across countries, by ISCO 2-digit occupation",
    x = "Occupation (ISCO 2008, 2-digit)",
    y = "Telework",
    caption = "Source EU-LFS. Employed population, excluding armed forces."
  )

ggsave("Figures/Variation_telework_occupation_boxplot.pdf", height = 6, width = 12)


# Choropleth of teleworking rates for specific occupations ----

# Homeworking rates by 2-digit occupation and NUTS-2 region
homework_occup_region <- LFS %>%
  # Aggregate at ISCO 2-digit level
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
  # Exclude ISCO 2-digit codes ending in 0 (which are actually 1-digit codes)
  filter(!is.na(isco_2d_code), !str_detect(isco_2d_code, "0$"), ) %>%  
  group_by(year, reg, isco_2d_code) %>% 
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  rename(NUTS_ID = reg) %>%
  # Manually fix some NUTS codes for AT, DE, NL
  mutate(
    # Strip last 0 from AT and DE, which are not in the NUTS specs
    NUTS_ID = str_replace_all(NUTS_ID, "(?<=AT\\d)0", ""),
    NUTS_ID = str_replace_all(NUTS_ID, "(?<=DE.)0", ""),
    NUTS_ID = str_replace_all(NUTS_ID, "NL00", "NL")
  )

# how many observations per region?
homework_occup_region$n_obs %>% summary()

# Join values for regional telework with NUTS maps
map_homework_occup_region <- inner_join(map_nuts, homework_occup_region, by = "NUTS_ID", multiple = "all") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE"))


occup_homework_freq <- LFS %>%
  # Exclude non-responses, missing, and armed forces
  filter(!isco1d %in% "Non response", !is.na(isco1d), isco1d != "Armed forces") %>%
  # Aggregate at ISCO 2-digit level
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
  # Exclude ISCO 2-digit codes ending in 0 (which are actually 1-digit codes)
  filter(!is.na(isco_2d_code), !str_detect(isco_2d_code, "0$"), ) %>%
  group_by(year, reglab, isco_2d_code) %>%
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco_2d_code" = "code")) %>% 
  arrange(isco_2d_code) %>% 
  mutate(
    occupation_label = paste0(isco_2d_code, ": ", occupation),
    occupation_label = factor(occupation_label) %>% fct_inorder() %>% fct_rev()
  )

occup_homework_freq


map_homework_occup_region %>%
  filter(isco_2d_code == "24") %>% 
  filter(n_obs > 30) %>%
  ggplot() +
  # Plot the country boundaries NUTS-0 in light grey in background
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0) %>% crossing(year = c(2018:2021)) %>% sf::st_as_sf(), fill = "grey70") +
  # Plot the index values on top
  geom_sf(aes(fill = homework_index)) +
  facet_wrap(. ~ year) +
  scale_fill_viridis_b("Telework\nindex") +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  # theme(legend.position = "top") +
  labs(
    title = "Telework index for ISCO 24: Business and Administration Professionals",
    subtitle = "Average values of the homeworking index by region",
    caption = "Telework index constructed from LFS;\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/telework_isco24_countries.pdf", height = 8, width = 9)







occup_homework_freq <- LFS %>%
  # Exclude non-responses, missing, and armed forces
  filter(!isco1d %in% "Non response", !is.na(isco1d), isco1d != "Armed forces") %>%
  # Aggregate at ISCO 2-digit level
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
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
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco_2d_code" = "code")) %>% 
  arrange(isco_2d_code) %>% 
  mutate(
    occupation_label = paste0(isco_2d_code, ": ", occupation),
    occupation_label = factor(occupation_label) %>% fct_inorder() %>% fct_rev()
  )

map_homework_occup_region <- inner_join(map_nuts, occup_homework_freq, by = "NUTS_ID", multiple = "all") %>%
  select(-id, -LEVL_CODE, -NUTS_NAME, -FID, -geo, -ends_with("_TYPE"))

