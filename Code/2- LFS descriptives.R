library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("janitor") # convenience functions to clean and adorn summary tables

library("sf") # Simple Features format of maps-as-tables

library("mapview") # Interactive maps
library("leafem") # Decorate interactive maps
library("leaflet.extras2") # Extra utilities to combine interactive maps
library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)
theme_set(theme_minimal()) # set minimalist theme as default for ggplot


# Indicators from LFS -----------------------------------------------------

LFS <- read_feather("../Data/lfs.feather")

# Respondents to LFS by country, over time
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

# Tabulate share of population living in cities, towns, and rural areas
LFS %>%
  group_by(reglab, degurba) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "degurba", values_from = "total") %>%
  adorn_percentages() %>%
  adorn_pct_formatting()

# Tabulate share of population living in cities, towns, and rural areas
LFS %>%
  group_by(reglab, urbrur) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "urbrur", values_from = "total") %>%
  adorn_percentages() %>%
  adorn_pct_formatting()

# Tabulate occupational structure
LFS %>%
  filter(empstat == "Employed", !isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(country, isco1d) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "country", values_from = "total") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting()


LFS_country_occup <- LFS %>%
  filter(year == 2021) %>%
  filter(empstat == "Employed", !isco1d %in% "Non response", !is.na(isco1d)) %>%
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
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Occupational structure of EU countries",
    subtitle = "Share of employed population in ISCO 1-digit occupation groups",
    caption = "Source, EU-LFS"
  )

occup_hw_freq <- LFS %>%
  filter(empstat == "Employed", !isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(year, country, isco08_3d) %>%
  summarise(
    hw_never = sum((homework == "Person never works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    hw_some  = sum((homework == "Person sometimes works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    hw_main  = sum((homework == "Person mainly works at home") * coeffy) / sum(coeffy, na.rm = TRUE)
  )

# Tabulate share of population living in cities, towns, and rural areas
LFS %>%
  group_by(reglab, urbrur) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  )

occup_hw %>%
  filter(year == 2019) %>%
  mutate(hw_any = 1 - hw_never) %>%
  group_by(isco08_3d) %>%
  summarise(
    hw_any_min = min(hw_any, na.rm = TRUE),
    hw_any_max = max(hw_any, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = isco08_3d, ymin = hw_any_min, ymax = hw_any_max)) +
  geom_errorbar() +
  coord_flip() +
  scale_x_reverse() +
  labs(
    x = "Share"
  )

# NUTS-2 and degree of urbanisation
LFS %>%
  count(reglab, degurba) %>%
  pivot_wider(names_from = degurba, values_from = n)

LFS %>%
  group_by(reglab, degurba) %>%
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = degurba, values_from = total) %>%
  adorn_percentages() %>%
  adorn_pct_formatting()
