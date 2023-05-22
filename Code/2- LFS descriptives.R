# Load common packages
source("Code/0- Load packages.R")


# Load LFS data, already encoded
LFS <- read_feather("Data/LFS.feather")

# Load ISCO-08 labels
labels_isco <- read_tsv("Metadata/ISCO-08.txt") %>% 
  mutate(occup_group = if_else(str_length(code) == 1, occupation, NA_character_)) %>% 
  fill(occup_group, .direction = "down") %>% 
  mutate(occup_group = factor(occup_group) %>% fct_inorder())

write_rds(labels_isco, "Metadata/labels_isco.rds")


# coeffy by country -------------------------------------------------------

#TODO: why coeffy missing (NL in 2021, FI, DK all years?)
LFS %>%
  group_by(country, year) %>%
  summarise(miss_weights = sum(is.na(coeffy)/n())*100 ) %>%
  filter(miss_weights > 0.01) %>% 
  pivot_wider(values_from = miss_weights, names_from = year, values_fill = 0) %>%
  view("% missing weights") %>% 
  xtable(.)
  


# Plot LFS respondents by country, over time ------------------------------
LFS %>%
  group_by(year, country) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  ggplot(aes(x = year, y = n_obs)) +
  geom_point() +
  geom_line() +
  facet_geo(~country, grid = eu_grid) +
  # scale_y_continuous(labels = comma_format()) +
  scale_y_log10(labels = comma_format()) +
  labs(
    title = "Number of respondents to the LFS by country, over the years", 
    y = "Number of respondents (log scale)",
    x = NULL
  )

ggsave("Figures/LFS_respondents_time.pdf", height = 8, width = 11)


# Tabulate degurba --------------------------------------------------------
# share of population living in cities, towns, or rural areas

LFS_pop_degurba <- LFS %>%
  group_by(reglab, degurba) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "degurba", values_from = "total") %>%
  adorn_percentages()

# write_xlsx(LFS_pop_degurba, "Tables/LFS_pop_degurba.xlsx")

LFS_pop_degurba %>%
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
# write_xlsx("Tables/LFS_pop_urbur.xlsx")


# ISCO codes by country ---------------------------------------------------

# number of respondents by ISCO 3-digit
LFS %>%
  count(isco_3d_code, country) %>% 
  pivot_wider(names_from = country, values_from = n) %>% 
  view("ISCO 3-digit codes by country")

# number of respondents by ISCO 2-digit
LFS %>% 
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
  count(isco_2d_code, country) %>% 
  pivot_wider(names_from = country, values_from = n) %>% 
  view("ISCO 2-digit codes")

# 2-digit codes ending with 0 are in fact 1-digit codes.
# Used in Malta (which doesn't have actual 2-digit codes), and a few other places
LFS %>% 
  mutate(isco_2d_code = str_sub(isco_3d_code, 1,2)) %>%
  filter(str_detect(isco_2d_code, "0$")) %>%
  count(isco_2d_code, country) %>% 
  pivot_wider(names_from = country, values_from = n) %>% 
  view("ISCO codes by country ending with 0")

# Look for ISCO codes with fewer than 3 digits
LFS %>%
  distinct(isco08_3d) %>%
  filter(str_length(isco08_3d) < 3) 

# Those are armed forces (ISCO 0)
LFS %>%
  distinct(country, isco08_3d, .keep_all = TRUE) %>% 
  filter(isco08_3d %in% c(0, 10, 11, 20, 30, 21, 31)) %>%
  select(year, country, isco08_3d, isco2d, isco1d)



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

LFS_occup_structure <- LFS_country_occup %>%
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

ggsave("Figures/LFS_occup_structure.pdf", height = 8, width = 11)

