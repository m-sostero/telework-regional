# Load common packages
source("Code/0- Load packages.R")


# Load LFS data, already encoded
LFS <- read_feather("Data/LFS.feather")

# Load ISCO-08 labels
labels_isco <- read_tsv("Metadata/ISCO-08.txt") %>% 
  mutate(occup_group = if_else(str_length(code) == 1, occupation, NA_character_)) %>% 
  fill(occup_group, .direction = "down") %>% 
  mutate(occup_group = factor(occup_group) %>% fct_inorder())


# coeffy by country -------------------------------------------------------

#TODO: why coeffy missing (NL in 2021, FI, DK all years?)
LFS %>%
  group_by(country, year) %>%
  summarise(miss_weights = sum(is.na(coeffy)/n())) %>%
  filter(miss_weights > 0) %>% 
  pivot_wider(values_from = miss_weights, names_from = year, values_fill = 0) %>%
  mutate(across(`2018`:`2021`, ~ percent(., accuracy = 0.1))) %>%
  view()

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

