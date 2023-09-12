# Load common packages and labels ----
source("Code/0- Load common.R")


# Load data ---------------------------------------------------------------

# Load LFS data, encoded
LFS <- read_feather("Data/LFS.feather")

# LFS respondents by country, over time ------------------------------
LFS_respondents <- LFS %>%
  group_by(year, country) %>%
  summarise(n_obs = n(), .groups = "drop") %>% 
  left_join(labels_country, by = c("country" = "country_code"))

LFS_respondents %>%
  pivot_wider(names_from = year, values_from = n_obs) %>% 
  view("LFS respondents")
  # write_xlsx("Tables/LFS_respondents.xlsx")

LFS_respondents %>%
  ggplot(aes(x = year, y = n_obs)) +
  geom_point() +
  geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name") +
  # scale_y_continuous(labels = comma_format()) +
  scale_y_log10(labels = comma_format()) +
  theme(panel.spacing = unit(1, "lines")) +
  labs(
    title = "Number of respondents to the LFS by country, over the years", 
    y = "Number of respondents (log scale)",
    x = NULL
  )

ggsave("Figures/LFS_respondents_time.pdf", height = 8, width = 11)
ggsave("Figures/LFS_respondents_time.png", height = 8, width = 11, bg = "white")


# missing population weights (coeffy) by country --------------------------

#TODO: why coeffy missing (NL in 2021, FI, DK all years?)
LFS %>%
  group_by(country, year) %>%
  summarise(miss_weights = sum(is.na(coeffy)/n())*100, .groups = "drop") %>%
  filter(miss_weights > 1) %>% 
  pivot_wider(values_from = miss_weights, names_from = year, values_fill = 0) %>%
  view("% missing weights")
# write_xlsx("Tables/LFS_missing_weights.xlsx")
# xtable(.)



# Missing values on homeworking variable ----------------------------------

missing_homework_pop <- LFS %>%
  group_by(country, homework) %>%
  summarise(n_people = sum(coeffy, na.rm = TRUE)) %>%
  pivot_wider(names_from = homework, values_from = n_people, values_fill = 0) %>% 
  rename(Missing = `NA`)

missing_homework_resp <- LFS %>%
  count(country, homework) %>%
  pivot_wider(names_from = homework, values_from = n, values_fill = 0) %>% 
  rename(Missing = `NA`) 

missing_homework_pc <- LFS %>%
  mutate(homework = homework %>% fct_na_value_to_level("Missing")) %>% 
  group_by(year, country, homework) %>%
  summarise(n_people = sum(coeffy, na.rm = TRUE), .groups = "drop_last") %>% 
  mutate(pc_pop = n_people/sum(n_people)) %>%
  mutate(pc_pop = pc_pop %>% percent()) %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = homework, values_from = pc_pop, values_fill = "")

write_xlsx(
  list(Population =  missing_homework_pop, Respondents = missing_homework_resp),
  "Tables/Telework_intensity_response.xlsx"
  )
  



# Employees vs self-employed ----------------------------------------------

LFS %>% 
  group_by(country, year, stapro) %>%
  summarise(total = sum(coeffy, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = total, fill = stapro)) +
  geom_col() +
  scale_fill_discrete("Professional status") +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Employees and self-employed by country, over the years", 
    y = "Number of people",
    x = NULL
  )

ggsave("Figures/LFS_stapro_eu.pdf", height = 8, width = 11)
ggsave("Figures/LFS_stapro_eu.png", height = 8, width = 11, bg = "white")


# degurba --------------------------------------------------------
# share of population living in cities, towns, or rural areas

LFS_pop_degurba <- LFS %>%
  group_by(reglab, degurba) %>%
  summarise(
    total = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "degurba", values_from = "total") %>%
  adorn_percentages()

# write_xlsx(LFS_pop_degurba, "Tables/LFS_population_degurba.xlsx")

LFS_pop_degurba %>%
  adorn_pct_formatting() %>%
  view("degurba by NUTS-2")


# urbrur ---------------------------------------------------------
# Total population by NUTS-2, with prevailing urban-rural region type

LFS %>%
  group_by(reglab, urbrur) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  )
# write_xlsx("Tables/LFS_population_urbur.xlsx")


# ISCO codes by country ---------------------------------------------------

# number of respondents by ISCO 3-digit
LFS %>%
  count(isco_3d_code, country) %>% 
  pivot_wider(names_from = country, values_from = n, names_sort = TRUE) %>% 
  left_join(labels_isco, by = c("isco_3d_code" = "code")) %>% 
  select(isco_3d_code, occupation, everything(), -occup_group) %>% 
  # write_xlsx("Tables/LFS_respondents_ISCO3d.xlsx")
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

# Those are armed forces (ISCO 0), they have been corrected in isco_3d_code
LFS %>%
  distinct(country, isco08_3d, isco_3d_code, .keep_all = TRUE) %>% 
  filter(isco08_3d %in% c(0, 10, 11, 20, 30, 21, 31)) %>%
  select(year, country, isco08_3d, isco_3d_code, isco2d, isco1d)



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
  # write_xlsx("Tables/LFS_occup_ISCO3d.xlsx")
  adorn_pct_formatting()



# Plot occupational structure by country ----------------------------------

LFS_country_occup <- LFS %>%
  filter(year == 2021) %>%
  filter(!isco1d %in% "Non response", !is.na(isco1d)) %>%
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
    subtitle = "Share of working population in ISCO 1-digit occupation groups (employees and self-employed)",
    y = "Share of total working population",
    caption = "Source: EU-LFS."
  )

ggsave("Figures/LFS_occup_structure.pdf", height = 8, width = 11)
ggsave("Figures/LFS_occup_structure.png", height = 8, width = 11, bg = "white")

