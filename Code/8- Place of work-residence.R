# Load common packages and labels ----
source("Code/0- Load common.R")


# Load data ---------------------------------------------------------------

# Labour Force Survey microdata, cleaned
LFS <- read_feather("Data/LFS.feather") 

# Official region NUTS codes and names
labels_nuts <- read_rds("Data/map_nuts.rds") %>%
  as_tibble() %>% 
  select(-geometry) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

border_regions <- read_tsv("Metadata/NUTS Border regions.tsv") %>% 
  mutate(NUTS3_ID = str_replace_all(Code, "\\s", "")) %>% 
  mutate(NUTS2_ID = str_sub(NUTS3_ID, 1, 4)) %>% 
  group_by(NUTS2_ID) %>% 
  summarise(border = any(Border == "Border region"))


map_nuts %>%
  filter(nuts_year == 2021, LEVL_CODE == 2) %>% 
  left_join(border_regions, by = c("NUTS_ID" = "NUTS2_ID")) %>% 
  ggplot() +
  geom_sf(aes(fill = border)) +
  geom_sf(data = map_nuts %>% filter(LEVL_CODE == 0, nuts_year == 2021) %>% sf::st_as_sf(), color = "black", fill = NA) +
  coord_sf(xlim = c(2.3e+6, 6.3e+6), ylim = c(5.4e+6, 1.4e+6), crs = sf::st_crs(3035), datum = NA) +
  scale_fill_manual("Border region", values = c("grey90", "#377EB8", "#E41A1C"), labels = c("Interior", "Border", "Not Applicable"))+
  theme_bw() 

# Summary statistics of place of work -----------------------------------------
work_location <- LFS %>%
  group_by(country, year, work_location) %>% 
  summarise(
    n_people = sum(coeffy, na.rm = TRUE), .groups = "drop",
    n_obs = n()
  ) %>% 
  complete(country, year, work_location, fill = list(n_people = 0, n_obs = 0)) %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  relocate(country_name, .after = country)


# Export summary statistics
# write_xlsx(
#   list(
#     Population = work_location %>% select(-n_obs) %>% pivot_wider(names_from = work_location, values_from = n_people),
#     Respondents = work_location %>% select(-n_people) %>% pivot_wider(names_from = work_location, values_from = n_obs)
#   ), 
#   "Tables/LFS_work_location.xlsx"
# )

# Plot share of people working outside region of residence -------------------
work_location %>% 
  group_by(year, country) %>%
  mutate(share_people = n_people/sum(n_people)) %>%
  ungroup() %>%
  filter(work_location %in% c("Other region in country of residence", "Other country", "Not stated")) %>% 
  mutate(
    year = factor(year),
    work_location = str_wrap(work_location, 20) %>% fct_rev()
    ) %>% 
  ggplot(aes(x = year, y = share_people, group = work_location, color = work_location)) +
  geom_point(aes(size = n_obs)) + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual("Place of work", values = c("#377EB8","#E41A1C", "#999999")) +
  scale_size_area("Number of respondents", breaks = c(100, 500, 1000, 2000, 3000, 5000, 10000), labels = comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1)) +
  labs(
    # title = "Share of employed people working outside region of residence, over time",
    x = "Year",
    y = "Share of employed people working outside region of residence\n(different scales)",
    caption = "source: EU Labour Force Survey, own elaboration\nRegions are NUTS-2 where available, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/LFS_residence_work_eu.pdf", height = 7, width = 10)
ggsave("Figures/LFS_residence_work_eu.png", height = 7, width = 10, bg = "white")


# Telework by location of work (region or country of work vs residence) ----

tw_location <- LFS %>% 
  filter(work_location != "Not stated") %>%
  group_by(year, country, work_location) %>% 
  mutate(total_group = sum(coeffy, na.rm = TRUE)) %>%  
  group_by(year, country, work_location, homework_any) %>%
  summarise(
    n_obs = n(),
    n_people = sum(coeffy, na.rm = TRUE),
    total_group = mean(total_group),
    .groups = "drop"
  ) %>% 
  filter(homework_any == 1) %>% 
  mutate(telework_share = n_people/total_group) %>%
  left_join(labels_country, by = c("country" = "country_code"))

tw_location %>% 
  mutate(
    year = factor(year),
    work_location = str_wrap(work_location, 20) %>% fct_rev()
    ) %>% 
  ggplot(aes(x = year, y = telework_share, group = work_location, color = work_location)) +
  geom_point() + geom_line() +
  # geom_point(aes(size = n_people)) + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") + 
  scale_color_brewer("Place of work", palette = "Set1", direction = -1) +
  scale_size_area("Number of respondents", breaks = c(10, 100, 500, 1000, 2000, 3000, 5000, 10000), labels = comma) +
  # guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust  = 1),
    legend.position = c(0.91, 0.5),
    legend.box.background = element_rect(colour = "grey40")
  ) +
  labs(
    # title = "Telework is not necessarily more common for those working in other regions or countries",
    # subtitle = "Teleworking frequency by respondent's place of work",
    # title = "Teleworking frequency by respondent's place of work",
    x = NULL,
    y = "Share of working population\n(different scales)",
    caption = "Source: EU Labour Force Survey, own elaboration\nRegions are mainly NUTS-2, NUTS-1 (AT and DE), or country (NL)"
  )

ggsave("Figures/Telework_residence_eu.pdf", height = 7, width = 10)
ggsave("Figures/Telework_residence_eu.png", height = 7, width = 10, bg = "white")

ggplot2::last_plot() + 
  labs(title = "Teleworking frequency by respondent's place of work", caption = NULL) +
  theme(
    legend.position = c(0.05, 0.05),
    legend.box.background = element_rect(colour = "grey40")
  )

ggsave("Figures/Telework_residence_eu.svg", height = 6, width = 8, bg = "white")





# Export table of sample size and rates of telework by work_location
tw_location %>% 
  arrange(country, year) %>% 
  select(-homework_any, -total_group, -n_people) %>% 
  relocate(country_name, .after = country) %>% 
  view()
  # write_xlsx("Tables/Telework_work_location.xlsx")


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

ggsave("Figures/LFS_work_abroad_eu.pdf", height = 8, width = 12)
ggsave("Figures/LFS_work_abroad_eu.png", height = 8, width = 12, bg = "white")

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
