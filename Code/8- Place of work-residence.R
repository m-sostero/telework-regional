# Load common packages and labels ----
source("Code/0- Load packages.R")


# Load data ---------------------------------------------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") 

# Official region NUTS codes and names
labels_nuts <- read_rds("Data/map_nuts.rds") %>%
  as_tibble() %>% 
  select(-geometry) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

# Summary statistics of place of work -----------------------------------------
work_location <- LFS %>%
  group_by(year, country, work_location) %>% 
  summarise(
    n_people = sum(coeffy, na.rm = TRUE), .groups = "drop",
    n_obs = n()
  ) %>% 
  complete(year, country, work_location, fill = list(n_people = 0, n_obs = 0))

# Number of observation by location
work_location %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = work_location, values_from = n_obs)

# Plot share of people working outside region of residence -------------------
work_location %>% 
  group_by(year, country) %>%
  mutate(share_people = n_people/sum(n_people)) %>%
  ungroup() %>%
  filter(work_location %in% c("Other region in country of residence", "Other country", "Not stated")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = share_people, group = work_location, color = work_location)) +
  geom_point(aes(size = n_obs)) + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual("Place of work", values = c("#377EB8","#E41A1C", "#999999")) +
  scale_size_area("Number of respondents", breaks = c(100, 500, 1000, 2000, 3000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "Share of employed people working outside region of residence, over time",
    x = "Year",
    y = "Share of employed people working outside region of residence\n(different scales)",
    caption = "source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/LFS_residence_work_eu.pdf", height = 8, width = 13)
ggsave("Figures/LFS_residence_work_eu.png", height = 8, width = 13, bg = "white")


# Telework by location of work (region or country of work vs residence) ----

hw_location <- LFS %>% 
  filter(work_location != "Not stated") %>%
  group_by(year, country, work_location) %>% 
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    n_obs = n(),
    population = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(labels_country, by = c("country" = "country_code"))


hw_location %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = homework_index, group = work_location, color = work_location)) +
  geom_point(aes(size = population)) + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") + 
  scale_color_brewer("Place of work", palette = "Set1", direction = -1) +
  scale_size_area("Number of people") + #, trans = "log10", breaks = c(10^c(1:5))) +
  # guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "Telework is not necessarily more common for those working in other regions or countries",
    subtitle = "Teleworking frequency by respondent's place of work",
    x = "Year",
    y = "Teleworking frequency index\n(different scales)",
    caption = "Source: EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_residence_eu.pdf", height = 8, width = 13)
ggsave("Figures/Telework_residence_eu.png", height = 8, width = 13, bg = "white")



# Country of residence vs work ---------------------------------

# Reclassify `ctryw` (country of work) as "Own country", "Abroad", NA
LFS <- LFS %>% 
  mutate(work_abroad = case_when(
    ctryw == "Work in own MS" ~ "Own country",
    ctryw %in% c("Work in another EU MS or UK", "Work in EEA", "Work in other European country", "Work in ROW", "Work in another country") ~ "Abroad",
    ctryw %in% c("Not stated", "Not applicable") | is.na(ctryw) ~ NA_character_
  ) %>% factor(levels = c("Own country", "Abroad"))
  )

# Share of people working abroad, by country ----

work_country <- LFS %>%
  mutate(work_abroad = fct_recode(work_location, "Own country" = "Region of residence", "Own country" = "Other region in country of residence")) %>% 
  group_by(year, country, stapro, work_abroad) %>% 
  summarise(
    n_people = sum(coeffy, na.rm = TRUE), .groups = "drop",
    n_obs = n()
  ) %>% 
  complete(year, country, work_abroad, fill = list(n_people = 0, n_obs = 0))

# Plot share of people working abroad
work_country %>% 
  group_by(year, country) %>%
  mutate(share_people = n_people/sum(n_people)) %>%
  ungroup() %>%
  filter(work_abroad == "Other country") %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = share_people, colour = stapro, group = stapro)) +
  geom_point(aes(size = n_obs)) + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_size_area("Number of respondents", breaks = c(100, 500, 1000, 2000, 3000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "Few people work abroad (under-sampled? COVID effect on response rates?)",
    subtitle = "Share of respondents people working abroad, by professional status",
    x = "Year",
    y = "Share of employed people working abroad\n(different scales)",
    source = "EU Labour Force Survey,\nown elaboration"
  )

ggsave("Figures/LFS_work_abroad_eu.pdf", height = 8, width = 13)
ggsave("Figures/LFS_work_abroad_eu.png", height = 8, width = 13, bg = "white")

# Count observations for people working abroad
work_country %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = work_abroad, values_from = n_obs) 



# Diagnostics: Check NUTS codes for `reg`, `regw`, `region_2d` ------------------------------

# Are any values of `reg` (region of residence) not valid EU NUTS codes?
LFS %>% 
  count(reg) %>%
  anti_join(labels_nuts, by = c("reg" = "NUTS_ID"))

# Are any values of `reg` (region of work) not valid EU NUTS codes?
LFS %>% 
  count(regw) %>%
  anti_join(labels_nuts, by = c("regw" = "NUTS_ID")) %>% 
  view()
# Yes, many country codes + 00


#TODO: Ask John if region_2d (residence) is indeed the original variable (not always at 2-digit)
# region_2dw is more often observed at 2-digits

LFS %>% 
  count(country, region_2d, reg) %>%
  view()

LFS %>% 
  count(country, reg, region_2d, regw, region_2dw) %>%
  arrange(country, desc(n)) %>% 
  view()

# In many cases, reg is 1-digit and regw is 2-digit,
# so we check whether the string of regw starts with that of reg
LFS %>% 
  count(country, reg, regw) %>% 
  mutate(
    reg_residence_work = if_else(str_detect(regw, paste0("^", reg)), TRUE, FALSE)
  ) %>% 
  arrange(country, desc(n))
