# Load common packages and labels ----
source("Code/0- Load common.R")


# Load data -------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") %>% 
  # Remove observations that did not answer to homework question
  filter(!is.na(homework)) %>% 
  # Remove observations with no sampling weights
  filter(!is.na(coeffy)) 

# Occupational teleworking indices
teleworkability <- read_dta("Data/Teleworkability indices.dta") %>%
  mutate(
    isco_3d_code = as.character(isco08_3d),
    physicalinteraction = physicalinteraction/1000
  )

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

# Telework share by degurba -----------------------------------------------

hw_degurba <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  compute_tw_share(year, country, degurba) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

# Share of telework by degurba
hw_degurba %>% 
  arrange(country, year, desc(degurba)) %>% 
  select(country, country_name, year, degurba, telework_share) %>% 
  pivot_wider(names_from = degurba, values_from = telework_share) %>% 
  rename_at(vars(Cities:`Rural areas`), .funs = ~ paste0("%TW\n", .)) %>%
  # view("Telework by degurba, weighted")
  write_xlsx("Tables/Telework_degurba.xlsx")


# Geofacet, grouped by degurba
hw_degurba %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = telework_share, group = degurba, color = degurba)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") + 
  # scale_color_manual("Degree of urbanisation", values = c("#33A02C", "#A6CEE3", "#1F78B4"), breaks = c("Rural areas", "Towns and suburbs", "Cities")) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "Telework has become more common, especially in cities, since 2020",
    subtitle = "Share of people teleworking by degree of urbanisation, over the years",
    x = "Year",
    y = "Share of people teleworking \n(different scales)",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_degurba_eu.pdf", height = 8, width = 13)
ggsave("Figures/Telework_degurba_eu.png", height = 8, width = 13, bg = "white")


# Zoom in on selected countries
selected_countries <- c("DE", "FR", "IT", "ES", "IE", "NL", "SE", "RO")

plot_hw_degurba <- hw_degurba %>% 
  filter(country %in% selected_countries) %>% 
  mutate(
    year = factor(year),
    country = factor(country, levels = selected_countries),
    ) %>% 
  arrange(country) %>% 
  mutate(country_name = fct_inorder(country_name)) %>% 
  ggplot(aes(x = year, y = telework_share, group = degurba, color = degurba)) +
  geom_point(aes(size = n_people), shape = 1) + geom_line() +
  facet_wrap(country_name ~ ., nrow = 2) + 
  # scale_color_manual(values = c("#33A02C", "#A6CEE3", "#1F78B4"), breaks = c("Rural areas", "Towns and suburbs", "Cities"))
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  scale_size_area("Population", labels = comma) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "Telework increased, particularly in cities, but country differences remain",
    subtitle = "Share of population teleworking by degree of urbanisation, over the years, selected countries",
    x = "Degree of urbanisation",
    y = "Share of population teleworking",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

plot_hw_degurba + guides(color = guide_legend(reverse = TRUE), size = guide_legend(label.hjust = 1)) 

ggsave("Figures/Telework_degurba_selected.pdf", height = 6, width = 9)
ggsave("Figures/Telework_degurba_selected.png", height = 6, width = 9, bg = "white")

ggplotly(plot_hw_degurba)


# Phase plot of regions
hw_degurba %>%
  filter(year %in% c(2019, 2021), degurba %in% c("Rural areas", "Cities")) %>% 
  pivot_wider(id_cols = c(country, country_name), names_from = c(degurba, year), values_from = telework_share) %>% 
  mutate(
    urban_delta = (`Cities_2021`-`Cities_2019`)/Cities_2019,
    rural_delta = (`Rural areas_2021`-`Rural areas_2019`)/`Rural areas_2019`
  ) %>% 
  ggplot(aes(x = urban_delta, y = rural_delta, label = country)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_label() +
  scale_x_continuous(labels = label_percent(prefix = "+")) +
  scale_y_continuous(labels = label_percent(prefix = "+")) +
  coord_equal() + 
  labs(
    title = "Telework increased faster in urban areas than in rural ones",
    x = "Relative change in telework\nin urban areas (2021 - 2019)",
    y = "Relative chance in telework\nin rural areas (2021 - 2019)"
  )

ggsave("Figures/Telework_changes_urban_rural.pdf", height = 6, width = 9)
ggsave("Figures/Telework_changes_urban_rural.png", height = 6, width = 9, bg = "white")



hw_degurba %>%
  filter(year %in% c(2019, 2021), degurba %in% c("Towns and suburbs", "Cities")) %>% 
  pivot_wider(id_cols = c(country, country_name), names_from = c(degurba, year), values_from = telework_share) %>% 
  mutate(
    urban_delta = (`Cities_2021`-`Cities_2019`)/Cities_2019,
    town_delta = (`Towns and suburbs_2021`-`Towns and suburbs_2019`)/`Towns and suburbs_2019`
  ) %>% 
  ggplot(aes(x = urban_delta, y = town_delta, label = country)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_label() +
  scale_x_continuous(labels = label_percent(prefix = "+")) +
  scale_y_continuous(labels = label_percent(prefix = "+")) +
  coord_equal()  + 
  labs(
    title = "Telework increased faster in urban areas than in rural ones",
    x = "Relative change in telework\nin urban areas (2021 - 2019)",
    y = "Relative chance in telework\nin towns and suburbs (2021 - 2019)"
  )

ggsave("Figures/Telework_changes_urban_towns.pdf", height = 6, width = 9)
ggsave("Figures/Telework_changes_urban_towns.png", height = 6, width = 9, bg = "white")


# Telework intensity by degurba -----------------------------

LFS_total <- LFS %>%
  group_by(year, country) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop") 

hw_degurba_freq <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  group_by(year, country, degurba, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_total, by = c("year", "country")) %>% 
  mutate(
    share = total / total_pop,
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home") %>% fct_rev()
  ) %>% 
  left_join(labels_country, by = c("country" = "country_code"))


# Telework intensity by degurba, selected countries -----
hw_degurba_freq %>%
  filter(country %in% selected_countries, !is.na(homework)) %>%
  mutate(year = factor(year), degurba = degurba %>% fct_rev()) %>%
  ggplot(aes(x = year, y = share, group = homework, fill = homework)) +
  geom_col(position = "stack") +
  facet_grid(degurba ~ country_name) +
  scale_fill_brewer("Telework", palette = "PuBu") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Rise in telework comes mostly from those doing so 'usually', rather than 'sometimes'",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Share of people employed\n(by country and year)"
  )

ggsave("Figures/Telework_intensity_degurba_selected.pdf", height = 6, width = 9)
ggsave("Figures/Telework_intensity_degurba_selected.png", height = 6, width = 9, bg = "white")


# Telework intensity by degurba in Ireland, number of people ----
hw_degurba_freq %>% 
  filter(country == "IE", !is.na(homework)) %>% 
  mutate(year = factor(year), degurba = fct_rev(degurba)) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) +
  geom_col(position = "stack") + 
  facet_wrap( ~ degurba) +
  scale_fill_brewer("Telework", palette = "PuBu") +
  scale_y_continuous(labels = comma_format(accuracy = 1, scale = 1000)) +
  labs(
    title = "Ireland: Telework has increased much more than the fall of residents in cities",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Number of people employed"
  )

ggsave("Figures/Telework_intensity_degurba_ireland.pdf", height = 6, width = 9)
ggsave("Figures/Telework_intensity_degurba_ireland.png", height = 6, width = 9, bg = "white")


# Telework share by urbrur ----

hw_urbrur <- LFS %>%  
  mutate(urbrur = replace_na(urbrur, "Regions undifferentiated") %>% fct_rev()) %>% 
  compute_tw_share(year, country, urbrur) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

hw_urbrur %>%
  pivot_wider(names_from = urbrur, values_from = telework_share) %>% 
  select(year, country, country_name, rev(levels(hw_urbrur$urbrur))) %>% 
  rename_at(vars(`Capital region`:`Regions undifferentiated`), .funs = ~ paste0("%TW\n", .)) %>% 
  # write_xlsx("Tables/Telework_urbrur.xlsx")
  view("Telework by urbrur")

# Geofacet, grouped by urbrur
hw_urbrur %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = telework_share, group = urbrur, color = urbrur)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_color_manual("Type of region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Regions undifferentiated")
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Capital regions have increasingly the highest rates of telework",
    subtitle = "Share of population teleworking by type of region, over the years",
    x = "Year",
    y = "Share of people teleworking\n(different scales)",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_urbrur_eu.pdf", height = 8, width = 11)
ggsave("Figures/Telework_urbrur_eu.png", height = 8, width = 11, bg = "white")


# Zoom in on selected countries, see "capital premium"

selected_countries <- c("DE", "FR", "IT", "ES", "IE", "NL", "SE", "RO")

hw_urbrur %>%
  filter(country %in% selected_countries) %>% 
  mutate(
    year = factor(year),
    country = factor(country, levels = selected_countries),
  ) %>% 
  arrange(country) %>% 
  mutate(country_name = fct_inorder(country_name)) %>% 
  ggplot(aes(x = year, y = telework_share, group = urbrur, color = urbrur)) +
  geom_point() + geom_line() +
  facet_wrap(~ country, nrow = 2) +
  scale_color_manual("Type of region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Regions undifferentiated")
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Signigicant cross-country differences remain in rates of telework",
    subtitle = "Share of population teleworking, selected countries",
    x = "Degree of urbanisation",
    y = "Share of population teleworking",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_urbrur_selected.pdf", height = 6, width = 9)
ggsave("Figures/Telework_urbrur_selected.png", height = 6, width = 9, bg = "white")


# Telework intensity by urbrur ----

hw_urbrur_freq <- LFS %>%
  mutate(urbrur = fct_rev(urbrur)) %>% 
  group_by(year, country, urbrur, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_total, by = c("year", "country")) %>% 
  mutate(
    share = total / total_pop,
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home") %>% fct_rev()
  ) %>% 
  left_join(labels_country, by = c("country" = "country_code"))


hw_degurba_freq <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  group_by(year, country, degurba, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_total, by = c("year", "country")) %>% 
  mutate(
    share = total / total_pop,
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home") %>% fct_rev()
  ) %>% 
  left_join(labels_country, by = c("country" = "country_code"))


# Telework intensity by urbrur, selected countries
hw_urbrur_freq %>%
  filter(country %in% selected_countries, country != "NL", !is.na(homework)) %>%
  mutate(year = factor(year), urbrur = urbrur %>% fct_rev()) %>%
  ggplot(aes(x = year, y = share, group = homework, fill = homework)) +
  geom_col(position = "stack") +
  facet_grid(urbrur ~ country_name) +
  scale_fill_brewer("Work at home", palette = "PuBu") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Rise in telework comes mostly from those doing so 'usually', rather than 'sometimes'",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Share of people employed\n(by country and year)"
  )

ggsave("Figures/Telework_intensity_urbrur_selected.pdf", height = 6, width = 9)
ggsave("Figures/Telework_intensity_urbrur_selected.png", height = 6, width = 9, bg = "white")


# Telework(ability) by degurba --------------------------------------------

tw_hw_degurba <- LFS %>%
  left_join(teleworkability, by = "isco_3d_code") %>% 
  group_by(year, country, reglab, degurba, stapro) %>% 
  summarise(
    telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    teleworkability = weighted.mean(physicalinteraction, wt = coeffy, na.rm = TRUE),
    coeffy = sum(coeffy, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) 

write_rds(tw_hw_degurba, "Data/tw_hw_degurba.rds")

tw_hw_degurba %>%  
  ggplot(aes(x = teleworkability, y = telework_share, size = coeffy, label = reglab, group = stapro, color = stapro)) +
  geom_point(aes(color = stapro), shape = 1, alpha = 0.3) +
  geom_smooth(method = lm, mapping = aes(weight = coeffy)) +
  scale_size_area() +
  facet_grid(degurba ~ year) +
  scale_color_brewer("Professional status", palette = "Set1") +
  scale_y_continuous(labels = percent_format()) +
  coord_equal() +
  guides(size = "none") +
  theme(
    legend.position = "top",
    panel.spacing = unit(1, "lines")
    ) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(
    title = "Increasing correlation between teleworkability and homeworking, similar across location types",
    subtitle = "Correlation between physical teleworkability and actual telework for EU27 NUTS-2 regions",
    x = "Phyisical teleworkability index", y = "Share of people teleworking"
  )

ggsave("Figures/Regional_correlation_teleworkability_telework_degurba.pdf", height = 6, width = 9)
ggsave("Figures/Regional_correlation_teleworkability_telework_degurba.png", height = 6, width = 9, bg = "white")

