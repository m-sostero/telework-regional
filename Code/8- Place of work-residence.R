# Load common packages and labels ----
source("Code/0- Load packages.R")


# Load data ---------------------------------------------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") %>% 
  # Code empty region of work as explicit missing value
  mutate(regw = na_if(regw, ""))

# Official region NUTS codes and names
labels_nuts <- read_rds("Data/map_nuts.rds") %>%
  as_tibble() %>% 
  select(-geometry) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)


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
  group_by(year, country, work_abroad) %>% 
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
  filter(work_abroad == "Abroad") %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = share_people, group = country)) +
  geom_point(aes(size = n_obs)) + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_size_area("Number of respondents", breaks = c(100, 500, 1000, 2000, 3000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "?",
    subtitle = "Share of employed people working abroad, by country, over time",
    x = "Year",
    y = "Share of employed people working abroad",
    source = "EU Labour Force Survey,\nown elaboration"
  )

ggsave("Figures/LFS_work_abroad_eu.pdf", height = 8, width = 13)
ggsave("Figures/LFS_work_abroad_eu.png", height = 8, width = 13, bg = "white")

# Count observations for people working abroad
work_country %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = work_abroad, values_from = n_obs) 


# Region of work vs residence ---------------------------------------------

# Encode variable if  region of residence is the same as the region of residence
# (or includes it, in case the granularity of reg < regw)
work_region <- LFS %>% 
  mutate(work_in_region = if_else(str_detect(regw, paste0("^", reg)), TRUE, FALSE)) %>% 
  count(country, reg, regw, work_in_region) %>% 
  arrange(country, desc(n)) %>% 
  rename(n_obs = n)

view(work_region)

write_xlsx(work_region, "Tables/LFS_work_in_region.xlsx")


# Telework by place of work vs residence ----------------------------------

# code work_location as the region or country of work vs residence
LFS <- LFS %>% 
  mutate(
    work_location = case_when(
      # region of residence is the same as the region of residence (or includes it, in case the granularity of reg < regw)
      str_detect(regw, paste0("^", reg)) ~ "Region of residence",
      ctryw == "Work in own MS" & !str_detect(regw, paste0("^", reg)) ~ "Other region in country of residence",
      ctryw %in% c("Work in another EU MS or UK", "Work in EEA", "Work in other European country", "Work in ROW", "Work in another country") ~ "Other country",
      is.na(regw) | is.na(ctryw) | ctryw == "Not stated" ~ "Not stated"
    ) %>% factor(levels = c("Region of residence", "Other region in country of residence", "Other country", "Not stated"))
  )

# Summary statistics of place of work
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

# Plot share of people working abroad, or not stated
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
    title = "?",
    subtitle = "Share of employed people working outside region of residence, over time",
    x = "Year",
    y = "Share of employed people working outside region of residence\n(different scales)",
    source = "EU Labour Force Survey,\n own elaboration"
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
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") + 
  scale_color_brewer("Place of work", palette = "Set1", direction = -1) +
  # guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "?",
    subtitle = "Teleworking frequency by respondent's place of work",
    x = "Year",
    y = "Teleworking frequency index\n(different scales)",
    source = "EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Telework_residence_eu.pdf", height = 8, width = 13)
ggsave("Figures/Telework_residence_eu.png", height = 8, width = 13, bg = "white")


# Check NUTS codes for `reg`, `regw`, `region_2d` ------------------------------

# Are any values of `reg` (region of residence) not valid NUTS codes?
LFS %>% 
  count(reg) %>%
  anti_join(map_nuts, by = c("reg" = "NUTS_ID"))

# Are any values of `reg` (region of work) not valid NUTS codes?
LFS %>% 
  count(regw) %>%
  anti_join(map_nuts, by = c("regw" = "NUTS_ID")) %>% 
  view()
# Yes, many country codes + 00

# How many cases of work outside their region of residence?
LFS %>% 
  count(reg, regw) %>% 
  filter(reg != regw) %>% 
  arrange(desc(n)) %>% 
  view()

#TODO: Ask John if region_2d (residence) is indeed the original variable (not always at 2-digit)
# region_2dw is more often observed at 2-digits

LFS %>% 
  count(country, region_2d, reg) %>%
  view()

LFS %>% 
  count(country, region_2d, region_2dw) %>%
  arrange(country, desc(n)) %>% 
  view()

# In many cases, reg is 1-digit and regw is 2-digit,
# so we check whether the string of regw starts with that of reg
reg_residence_work <- LFS %>% 
  count(country, reg, regw) %>% 
  mutate(
    reg_residence_work = if_else(str_detect(regw, paste0("^", reg)), TRUE, FALSE)
  ) %>% 
  arrange(country, desc(n))
