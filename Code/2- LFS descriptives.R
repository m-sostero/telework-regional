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

LFS_population <- LFS %>%
  group_by(year, country) %>%
  summarise(pop = sum(coeffy, na.rm = TRUE), .groups = "drop") %>% 
  left_join(labels_country, by = c("country" = "country_code")) %>% 
  pivot_wider(names_from = year, values_from = pop) 

LFS_respondents %>%
  ggplot(aes(x = year, y = n_obs)) +
  geom_point() +
  geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = comma_format(),  limits = c(1, NA)) +
  # scale_y_log10(labels = comma_format(), limits = c(1, NA)) +
  theme(
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
  labs(
    title = "Number of respondents to the LFS by country, over the years", 
    y = "Number of respondents (different scales)",
    x = NULL
  )

ggsave("Figures/LFS_respondents_time.pdf", height = 8, width = 11)
ggsave("Figures/LFS_respondents_time.png", height = 8, width = 11, bg = "white")

LFS_respondents_change <- LFS_respondents %>%
  pivot_wider(names_from = year, values_from = n_obs) %>% 
  rowwise() %>% 
  mutate(
    delta_19_20 = `2020`/`2019` - 1,
    delta_22_19 = `2022`/`2019` - 1,
    )

write_xlsx(
  list(Population = LFS_population, Respondents = LFS_respondents_change),
  "Tables/LFS_respondents_population.xlsx"
)


# missing population weights (coeffy) by country --------------------------
# Why is coeffy missing (NL in 2021, 2022, FI, DK all years?)
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
  # mutate(pc_pop = pc_pop %>% percent()) %>% 
  select(-n_people) %>% 
  pivot_wider(names_from = homework, values_from = pc_pop, values_fill = 0)

write_xlsx(
  list(Population =  missing_homework_pop, Respondents = missing_homework_resp),
  "Tables/Telework_intensity_response.xlsx"
  )
  

# Telework by country --------------------------------------------

LFS_total <- LFS %>%
  group_by(year, country) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop") 

# Compute EU totals 
LFS %>%
  filter(!is.na(homework_any)) %>% 
  group_by(year, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>% 
  spread(homework, total) 

hw_freq <- LFS %>%
  group_by(year, country, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_total, by = c("year", "country")) %>% 
  mutate(
    share = total / total_pop,
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home") %>% fct_rev()
  ) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

hw_freq %>%
  filter(homework != "never") %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = share, group = homework, fill = homework)) +
  geom_col(position = "stack") +
  facet_geo(~ country, grid = eu_grid, label = "name") +
  scale_fill_manual("Work from home", values = c("#9ECAE1", "#2171B5"), breaks = c("sometimes", "usually")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.60)) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = c(0.92, 0.5),
    legend.box.background = element_rect(colour = "grey40")
    ) +
  labs(
    title = "Frequency of work from home, by country and year",
    y = "Share of working population", x = NULL,
    caption = "Source: EU-LFS."
  )

ggsave("Figures/Telework_intensity_eu.pdf", height = 7, width = 9)
ggsave("Figures/Telework_intensity_eu.png", height = 7, width = 10, bg = "white")

ggplot2::last_plot() +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.6)) +
  theme(
    legend.position = c(0.05, 0.05),
    legend.box.background = element_rect(colour = "grey40")
  ) +
  labs(title = NULL)
ggsave("Figures/Telework_intensity_eu.svg", height = 6, width = 8, bg = "white")


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
# share of population pre-COVID living in cities, towns, or rural areas

LFS_pop_degurba <- LFS %>%
  filter(year == 2019) %>% 
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
# Total population by NUTS-2 pre-COVID, with prevailing urban-rural region type

LFS %>%
  filter(year == 2019) %>% 
  group_by(reglab, urbrur) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) # %>% write_xlsx("Tables/LFS_population_urbur.xlsx")


# ISCO codes by country ---------------------------------------------------
# NOTE: all years pooled! Used to show which country/ISCO combination is missing

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



# Tabulate occupational structure pre-COVID -----------------------------------------

LFS %>%
  # Exclude occupation missing and non-responding
  filter(year == 2019, !isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(country, isco1d) %>%
  summarise(
    total = sum(coeffy, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "country", values_from = "total") %>% 
  adorn_percentages(denominator = "col") %>%
  write_xlsx("Tables/LFS_occup_ISCO3d.xlsx")
  adorn_pct_formatting()



# Plot occupational structure by country ----------------------------------

LFS_country_occup <- LFS %>%
  filter(year == 2019) %>%
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


# Employment by rates by URBRUR -----------------------------------------------
# Compute from original LFS data, to include those not in employment

# Load table of NUTS-URBRUR mapping, byt John
reg_urbrur <- read_dta("R:/SpssStata/EJM_DO_NOT_DELETE/annualreports/REGIONAL 2023/data/NUTS_urbanrural/urbrurmerge2.dta") %>%
  as_factor() 

# Load raw LFS microdata, from server, only for selected years
LFS_raw <- list.files("R:/SpssStata/EJM_DO_NOT_DELETE/DATA/LFS_For_Researchers_1983to2022/CSV_LFS", full.names = TRUE) %>% 
  # Import only variables of interest
  map_dfr(~ read_csv(., col_types = cols_only(
    YEAR = "i",
    COUNTRY = "c",
    REGION_2D = "c",
    AGE_GRP = "c",
    EMPSTAT = "i",
    COEFFY = "d"
  ), show_col_types = FALSE))

# Export raw LFS data          
write_rds(LFS_raw, "Data/LFS_raw.rds")


# Minimally clean LFS
LFS_raw <- read_rds("Data/LFS_raw.rds")

LFS <- LFS_raw %>% 
  mutate(reg = paste0(COUNTRY, REGION_2D), REGION_2D = NULL) %>%
  left_join(reg_urbrur, by = "reg")

# Compute population by employment status by URBRUR
total_urbrur <- LFS %>% 
  # Exclude Norway
  filter(COUNTRY != "NO") %>% 
  # Keep only relevant age groups
  filter(AGE_GRP %in% c("Y20-24", "Y25-29", "Y30-34", "Y35-39", "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64")) %>% 
  # Code employment status (discard 9, which is unknown)
  filter(EMPSTAT != 9) %>% 
  mutate(empl = factor(EMPSTAT, levels = c(1,2), labels = c("Employed", "Not employed"))) %>%
  mutate(urbrur = fct_na_value_to_level(urbrur, "Entire country") %>% fct_recode("Entire country" = "Regions undifferentiated")) %>% 
  group_by(YEAR, urbrur, empl) %>% 
  summarise(people = sum(COEFFY, na.rm = TRUE), .groups = "drop") 

# Compute employment rate by urbrur
empl_rate_urbrur <- total_urbrur %>% 
  spread(empl, people) %>% 
  rowwise() %>% 
  mutate(empl_rate = c(`Employed` / (`Employed` + `Not employed`))) 

# Plot employment rate by urbrur, by country and year
empl_rate_urbrur %>% 
  ggplot(aes(x = YEAR, y = empl_rate, colour = urbrur)) +
  geom_point() + geom_line() +
  geom_dl(aes(label = urbrur), method = list(dl.trans(x = x + .3), "last.qp")) +
  scale_x_continuous(limits = c(2019, 2022.5), labels = c(2019:2022, "")) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(
    guide = NULL,
    "Type of region\n(urbrur)",
    values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99"), 
    breaks = c("Capital region", "Mainly urban", "Intermediate", "Mainly rural", "Entire country")
  ) +
  labs(
    title = "EU employment rate by region-type",
    subtitle = "People aged 20-64, by NUTS region-type",
    y = "Employment rate", x = "Year", 
    caption = "Regions are NUTS-2. 'Entire country' includes CY, EE, LU, LV, and MT,\n(which consist of a single NUTS-2 region) and NL, which is reported at the country level"
  )

ggsave("Figures/LFS_empl_urbrur.png", height = 6, width = 9, bg = "white")
ggsave("Figures/LFS_empl_urbrur.svg", height = 6, width = 9, bg = "white")


#TODO: Telework by country and age ----------------------------------
 
LFS %>% 
  mutate(year = factor(year)) %>% 
  group_by(year, country, age) %>% 
  summarise(telework_share = weighted.mean(homework_any, wt = coeffy, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = year, y = telework_share, group = age, color = age)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, label = "name", scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Telework has become more common among employees, catching up with the self-employed",
    subtitle = "Share of people teleworking at least some of the time, by professional status",
    y = "Share of people teleworking\n(different scales)"
  )

ggsave("Figures/Telework_stapro_eu.pdf", height = 8, width = 11)
ggsave("Figures/Telework_stapro_eu.png", height = 8, width = 11, bg = "white")

