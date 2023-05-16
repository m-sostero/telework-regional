library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("janitor") # convenience functions to clean and adorn summary tables

library("sf") # Simple Features format of maps-as-tables
library("geofacet") # Arrange plot facets according to (EU) map position
library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps
library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)

library("plotly") # Interactive plots

theme_set(theme_minimal()) # set minimalist theme as default for ggplot


# Load LFS data, already encoded
LFS <- read_feather("Data/LFS.feather")

# Load ISCO-08 labels
labels_isco <- read_tsv("Metadata/ISCO-08.txt") %>% 
  mutate(occup_group = if_else(str_length(code) == 1, occupation, NA_character_)) %>% 
  fill(occup_group, .direction = "down") %>% 
  mutate(occup_group = factor(occup_group) %>% fct_inorder())

# Plot LFS respondents by country, over time ------------------------------
LFS %>%
  group_by(year, country) %>%
  summarise(
    n_obs = n(),
    total_coeffy = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = n_obs)) +
  geom_point() +
  geom_line() +
  facet_geo(~country, grid = "eu_grid1") +
  scale_y_log10(labels = comma_format()) +
  labs(title = "Number of respondents to the LFS by country, over the years")


# Tabulate degurba --------------------------------------------------------
# share of population living in cities, towns, or rural areas

LFS %>%
  group_by(reglab, degurba) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "degurba", values_from = "total") %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  view("degurba by NUTS-2")


# Tabulate urbrur ---------------------------------------------------------
# Total population by NUTS-2, with prevailing urban-rural region type

LFS %>%
  group_by(reglab, urbrur) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  )

# Tabulate occupational structure -----------------------------------------
LFS %>%
  # Exclude occupation missing and non-responding
  filter(year == 2021, !isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(country, isco1d) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "country", values_from = "total") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting()


# Plot occupational structure by country ----------------------------------

LFS_country_occup <- LFS %>%
  filter(year == 2021) %>%
  filter( !isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(country, isco1d) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop_last"
  ) %>%
  mutate(
    country_total = sum(total, na.rm = T),
    share = total / country_total
  )

LFS_country_occup %>%
  ggplot(aes(x = country, y = share, fill = isco1d)) +
  geom_col() +
  scale_fill_brewer("Occupation (ISCO 2008, 1-digit)", palette = "Paired") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Occupational structure of EU countries",
    subtitle = "Share of employed population in ISCO 1-digit occupation groups",
    y = "Share of total employed population",
    caption = "Source: EU-LFS"
  )


# Telework frequencies by occupation, year, country -----------------------

occup_tw_freq <- LFS %>%
  filter(!isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(year, country, isco08_3d) %>%
  summarise(
    tw_never = sum((homework == "Person never works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    tw_some  = sum((homework == "Person sometimes works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    tw_main  = sum((homework == "Person mainly works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco08_3d" = "code"))

occup_tw_freq %>%
  filter(year == 2019) %>%
  mutate(tw_any = 1 - tw_never) %>%
  group_by(isco08_3d) %>%
  summarise(
    tw_any_min = min(tw_any, na.rm = TRUE),
    tw_any_max = max(tw_any, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = isco08_3d, ymin = tw_any_min, ymax = tw_any_max)) +
  geom_errorbar() +
  coord_flip() +
  scale_x_reverse() +
  labs(
    x = "Share"
  )

plot_occup_tw_freq <- occup_tw_freq %>%
  filter(year == 2019) %>%
  mutate(tw_any = 1 - tw_never) %>%
  group_by(isco08_3d, occupation, occup_group) %>%
  summarise(var_tw = var(tw_any,na.rm = TRUE) %>% round(3), .groups = "drop") %>%
  ggplot(aes(x = isco08_3d, y = var_tw, color = occup_group, label = occupation)) +
  geom_point() +
  coord_flip() +
  scale_x_reverse() +
  scale_color_brewer("Occupation\n(ISCO 2008, 1-digit)", palette = "Paired", na.value = "grey50") +
  labs(
    title = "How much does the rate of telework vary for the same occupation, across EU countries?",
    subtitle = "Variance in share of the of any telework, by occupation",
    x = "Occupation (ISCO 03, 3-digit)",
    y = "Variance in share of the of any telework"
  )

ggplotly(plot_occup_tw_freq)

