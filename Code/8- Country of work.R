# Load common packages and labels ----
source("Code/0- Load packages.R")


# Occupational teleworkability statistics ---------------------------------------

LFS <- read_feather("Data/LFS.feather") 

# Reclassify `ctryw` (country of work) as "Own country", "Abroad", NA
LFS <- LFS %>% 
  mutate(work_abroad = case_when(
    ctryw == "Work in own MS" ~ "Own country",
    ctryw %in% c("Work in another EU MS or UK", "Work in EEA", "Work in other European country", "Work in ROW", "Work in another country") ~ "Abroad",
    ctryw %in% c("Not stated", "Not applicable") | is.na(ctryw) ~ NA_character_
  ) %>% factor(levels = c("Own country", "Abroad"))
  )


# Share of people working abroad, by country ----

country_work <- LFS %>%
  group_by(year, country, work_abroad) %>% 
  summarise(
    n_people = sum(coeffy, na.rm = TRUE), .groups = "drop",
    n_obs = n()
    ) %>% 
  complete(year, country, work_abroad, fill = list(n_people = 0, n_obs = 0))

# Plot share of people working abroad
country_work %>% 
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
    source = "EU Labour Force Survey,\n own elaboration"
  )

ggsave("Figures/Work_abroad_EU.pdf", height = 8, width = 13)
ggsave("Figures/Work_abroad_EU.png", height = 8, width = 13, bg = "white")

# Count observations for people working abroad
country_work %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = work_abroad, values_from = n_obs) 


hw_abroad <- LFS %>% 
  filter(!is.na(work_abroad)) %>%
  group_by(year, country, work_abroad) %>% 
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)), 
    n_obs = n(),
    population = sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(labels_country, by = c("country" = "country_code"))
  

hw_abroad %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = homework_index, group = work_abroad, color = work_abroad)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") + 
  scale_color_brewer("Place of work", palette = "Set1", direction = -1) +
  # guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    title = "?",
    subtitle = "Teleworking frequency by respondent's country of work",
    x = "Year",
    y = "Teleworking frequency index\n(different scales)",
    source = "EU Labour Force Survey,\n own elaboration"
  )

