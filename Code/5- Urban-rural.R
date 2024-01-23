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


# Telework share by degurba -----------------------------------------------

# Compute %tw by country, year, and degurba
hw_degurba <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  compute_tw_share(year, country, degurba) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

# Compute EU average 
hw_degurba_EU <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  compute_tw_share(year, degurba) %>% 
  mutate(
    country = "EU-27",
    country_name = "European Union (27)",
  ) %>% 
  arrange(year, desc(degurba))


# Share of telework by degurba; EU and national figures
bind_rows(
  hw_degurba_EU,
  arrange(hw_degurba, country, year, desc(degurba))
)%>% 
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
    # title = "Telework has become more common, especially in cities, since 2020",
    # subtitle = "Share of people teleworking by degree of urbanisation, over the years",
    x = NULL,
    y = "Share of working population \n(different scales)",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_degurba_eu.pdf", height = 8, width = 13)
ggsave("Figures/Telework_degurba_eu.png", height = 7, width = 10, bg = "white")

ggplot2::last_plot() + labs(
  title = "Share of people teleworking by degree of urbanisation, over the years",
  x = NULL,
  y = "Share of working population \n(different scales)"
) + theme(
  legend.position = c(0.05, 0),
  legend.box.background = element_rect(colour = "grey40")
)

ggsave("Figures/Telework_degurba_eu.svg", height = 6, width = 8, bg = "white")


hw_degurba %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = telework_share, group = degurba, color = degurba)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") + 
  # scale_color_manual("Degree of urbanisation", values = c("#33A02C", "#A6CEE3", "#1F78B4"), breaks = c("Rural areas", "Towns and suburbs", "Cities")) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1), legend.position = "top") +
  labs(
    # title = "Telework has become more common, especially in cities, since 2020",
    title = "Share of people teleworking by degree of urbanisation, over the years",
    x = "Year",
    y = "Share of people teleworking \n(different scales)",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Presentation/Telework_degurba_eu.png", height = 8, width = 10, bg = "white")



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


# Telework by degurba at EU, pre-post --------------------------------------------

telework_degurba <- LFS %>%
  group_by(year, degurba) %>%
  summarise(
    total_pop = sum(coeffy, na.rm = TRUE),
    total_tw_sometimes = sum(coeffy[homework ==  "Person sometimes works at home"], na.rm = TRUE),
    total_tw_usually = sum(coeffy[homework ==  "Person mainly works at home"], na.rm = TRUE),
    Sometimes = total_tw_sometimes/total_pop,
    Usually = total_tw_usually/total_pop,
    tw_any = Sometimes + Usually,
    .groups = "drop"
  ) 

telework_degurba %>% 
  select(year, degurba, Sometimes, Usually) %>% 
  pivot_longer(cols = c(Sometimes, Usually), names_to = "frequency") %>%
  filter(year %in% c(2019, 2021)) %>% 
  ggplot(aes(x = degurba, y = value, fill = degurba)) +
  geom_col(aes(alpha = frequency), position = "stack") +
  facet_wrap(~ year) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer("Degree of urbanisation", palette = "Set2", direction = -1) +
  scale_alpha_manual("Work from home", breaks = c("Sometimes", "Usually"), values = c(0.4, 1)) +
  guides(alpha = guide_legend(order = 1), fill = guide_none()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    # title = "Share of employees working from home by territorial typology pre and post-COVID (2019 and 2021)",
    x = NULL, y = "Share of people working from home"
  )

ggsave("Figures/Telework_urbrur_pre_post.svg", height = 4, width = 8, bg = "white")


# Phase plot growth in cities vs rest ----

hw_cities_rest <- LFS %>%
  mutate(cities = degurba %>% fct_recode("Rest" = "Towns and suburbs", "Rest" = "Rural areas")) %>% 
  compute_tw_share(year, country, cities) %>% 
  left_join(labels_country, by = c("country" = "country_code"))


hw_cities_rest %>%
  filter(year %in% c(2019, 2021)) %>% 
  pivot_wider(id_cols = c(country, country_name), names_from = c(cities, year), values_from = telework_share) %>% 
  mutate(
    urban_delta = (`Cities_2021`-`Cities_2019`)/Cities_2019,
    rest_delta = (`Rest_2021`-`Rest_2019`)/`Rest_2019`
  ) %>% 
  ggplot(aes(x = urban_delta, y = rest_delta, label = country)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted",  color = "grey50") +
  geom_point() + geom_text_repel(seed = 1235) +
  annotate("text", x = 3.4, y = 3.7, label = "Larger increase\nin towns, suburbs and rural areas", color = "grey50", hjust = 1) +
  annotate("text", x = 4.1, y = 3.7, label = "Larger increase in cities", color = "grey50", hjust = 0) +
  scale_x_continuous(labels = label_percent(prefix = "+")) +
  scale_y_continuous(labels = label_percent(prefix = "+")) +
  coord_equal() + 
  labs(
    title = "Relative changes in telework 2019–2021: cities vs towns, suburbs, and rural areas",
    x = "Cities",
    y = "Towns, suburbs and rural areas areas"
  )

ggsave("Figures/Telework_changes_urban_rest.pdf", height = 5, width = 8.4)
ggsave("Figures/Telework_changes_urban_rest.png", height = 5, width = 8.4, bg = "white")



# Population relocation vs telework ---------------------------------------

# Calculate total population in cities vs rest by year
pop_cities_rest <- LFS %>%
  mutate(cities = degurba %>% fct_recode("Rest" = "Towns and suburbs", "Rest" = "Rural areas")) %>% 
  group_by(year, country, cities) %>%  
  summarise(pop = sum(coeffy, na.rm = TRUE), .groups = "drop")

# Combine %TW in cities/rest with total population in cities vs rest
#TODO: change reference year 
hw_cities_rest %>% 
  full_join(pop_cities_rest, by = c("year", "country", "cities")) %>%
  # compute differences 2019 2021
  filter(year %in% c(2019, 2021)) %>% 
  pivot_wider(id_cols = c(country, country_name), names_from = c(cities, year), values_from = c(telework_share, pop)) %>% 
  mutate(
    delta_pop_rest = (`pop_Rest_2021`-`pop_Rest_2019`) / pop_Rest_2019,
    delta_tw_rest  = (`telework_share_Rest_2021`-telework_share_Rest_2019)/telework_share_Rest_2019
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = delta_pop_rest, y = delta_tw_rest, label = country)) +
  geom_vline(xintercept = 0, linetype = "dotted",  color = "grey50") +
  geom_point() + geom_text_repel(seed = 1235) +
  annotate("text", x = -0.12, y = 3.7, label = "Population outside cities shrank", color = "grey50", hjust = 0) +
  annotate("text", x =  0.12, y = 3.7, label = "Population outside cities grew"  , color = "grey50", hjust = 1) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent(prefix = "+")) +
  geom_smooth() + 
  labs(
    title = "No evidence of relocation because of telework?",
    subtitle = "Changes in population and telework in in towns, suburbs, and rural areas between 2019 and 2021",
    x = "Relative changes in population",
    y = "Relative growth in telework"
  )

# Trends in population outside cities vs rest ------------------
 
# Normalise population levels in 2018 at 100, look at trends since
pop_cities_rest %>%
  pivot_wider(names_from = cities, values_from = pop) %>% 
  group_by(country) %>% 
  arrange(country, year) %>%
  mutate(
    Cities = (Cities / Cities[1])*100,
    Rest  = (Rest / Rest[1])*100
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Cities, Rest), names_to = "territory") %>% 
  mutate(territory = factor(territory, labels = c("Cities", "Towns, Suburbs, Rural areas"))) %>% 
  # Plot trends by country
  ggplot(aes(x = year, y = value, group = interaction(country, territory), color = territory)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = 100) + 
  scale_color_discrete("Territorial typology", direction = -1) +
  facet_geo(~ country, grid = eu_grid, label = "name") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = "top") +
  labs(
    title = "Changes in employed population by territorial typology",
    subtitle = "Estimated from LFS sampling weights, 2018 value = 100",
    x = "Year", y = "Population change"
  )

ggsave("Figures/Population_territory_prop_eu.png", height = 9, width = 9, bg = "white")

pop_cities_rest %>%
  mutate(territory = factor(cities, labels = c("Cities", "Towns, Suburbs, Rural areas"))) %>% 
  # Plot values by country
  ggplot(aes(x = year, y = pop, group = interaction(country, territory), color = territory)) +
  geom_point() + geom_line() +
  scale_color_discrete("Territorial typology", direction = -1) +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = "top") +
  labs(
    title = "Changes in employed population by territorial typology",
    subtitle = "Estimated from LFS sampling weights, absolute numbers",
    x = "Year", y = "Population (thousands)"
  )

ggsave("Figures/Population_territory_abs_eu.png", height = 9, width = 9, bg = "white")


# Teleworkabile employment by degurba over time --------------

# Aggregate actual telework (homework_any) and potential telework (homework_any) by degurba region
degurba_teleworkability <- LFS %>%
  inner_join(teleworkability, by = "isco08_3d") %>%
  group_by(country, year, degurba) %>% 
  summarise(
    share_hw = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    technical_tw = weighted.mean(physicalinteraction, wt = coeffy, na.rm = TRUE),
    pop = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  )

degurba_teleworkability %>%
  ggplot(aes(x = year, y = technical_tw, color = degurba)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Telework potential has remained relatively constant across territorial typologies",
    subtitle = "Average technical teleworkability by degree of urbanisation",
    x = "Year", y = "Teleworkability"
  ) 

#TODO: compute EU aggregate
degurba_teleworkability_eu <- LFS %>%
  inner_join(teleworkability, by = "isco08_3d") %>%
  group_by(year, degurba) %>% 
  summarise(
    share_hw = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    technical_tw = weighted.mean(physicalinteraction, wt = coeffy, na.rm = TRUE),
    pop = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  )

degurba_teleworkability_eu %>%
  ggplot(aes(x = year, y = technical_tw, color = degurba)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  labs(
    title = "Telework potential has remained relatively constant across territorial typologies",
    subtitle = "EU average technical teleworkability by degree of urbanisation",
    x = "Year", y = "Teleworkability"
  ) 


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


# Compute EU average 
hw_urbrur_EU <- LFS %>%
  filter(!is.na(urbrur)) %>% 
  mutate(urbrur = fct_rev(urbrur)) %>% 
  compute_tw_share(year, urbrur) %>% 
  mutate(
    country = "EU-27 (excl. NL)",
    country_name = "European Union (27), excluding NL",
  ) %>% 
  arrange(year, desc(urbrur))

# Share of telework by degurba; EU and national figures
bind_rows(
  hw_urbrur_EU,
  arrange(hw_urbrur, country, year, desc(urbrur))
) %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = urbrur, values_from = telework_share) %>% 
  select(year, country, country_name, rev(levels(hw_urbrur$urbrur))) %>% 
  rename_at(vars(`Capital region`:`Regions undifferentiated`), .funs = ~ paste0("%TW\n", .)) %>% 
  write_xlsx("Tables/Telework_urbrur.xlsx")
  # view("Telework by urbrur")

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
    # title = "Capital regions have increasingly the highest rates of telework",
    # subtitle = "Share of population teleworking by type of region, over the years",
    x = "Year",
    y = "Share of working population\n(different scales)",
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
  group_by(year, country, degurba) %>% 
  summarise(
    telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE),
    teleworkability = weighted.mean(physicalinteraction, wt = coeffy, na.rm = TRUE),
    coeffy = sum(coeffy, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>% 
  filter(!is.nan(teleworkability))

# Rate of teleworkability by degurba
tw_hw_degurba %>% 
  ggplot(aes(x = year, y = teleworkability, color = degurba)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name") +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  labs(
    title = "The occupational structure has remained mostly constant across territorial typologies",
    subtitle = "Average technical teleworkability by degree of urbanisation over the year",
    y = "Average technical teleworkability"
  )
  

tw_hw_degurba %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = country, y = teleworkability, fill = degurba)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_color_brewer("Degree of urbanisation", palette = "Set2") +
  labs(
    title = "The occupational structure has remained mostly constant across territorial typologies",
    subtitle = "Average technical teleworkability by degree of urbanisation over the year",
    y = "Average technical teleworkability"
  )

plot_ends <- tw_hw_degurba %>% 
  filter(year == 2019) %>%
  filter(degurba == "Rural areas")

tw_hw_degurba %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = degurba, y = teleworkability, group = country)) +
  geom_point(aes(colour = degurba)) + geom_line(alpha = 0.3) +
  geom_text_repel(data = plot_ends, aes(label = country)) +
  scale_color_brewer("Degree of urbanisation", palette = "Set2", direction = -1, guide = NULL) +
  labs(
    title = "Across EU, technical teleworkability is higher in cities than in towns or suburbs, than in rural areas",
    subtitle = "Average technical teleworkability by degree of urbanisation in 2019",
    y = "Average technical teleworkability", x = "Territorial typology"
  )
