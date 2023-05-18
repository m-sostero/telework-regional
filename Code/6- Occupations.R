library("tidyverse") # collection of packages for working with data
library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files


# Occupational teleworkability statistics ---------------------------------------

LFS <- read_feather("Data/LFS.feather") 

teleworkability <- read_dta("Data/Teleworkability indices.dta")

occupational_variables <- read_dta("Data/occup.dta") %>%
  as_factor() %>%
  arrange(country, year, isco08_3d)

# Teleworkability vs telework index ------

occupational_variables %>%
  filter(country %in% c("DE", "ES", "FR", "IE", "PL", "SE")) %>%
  ggplot(aes(x = physicalinteraction, y = homework_index, size = coeffy)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  scale_size_area() +
  facet_grid(country ~ year) +
  coord_equal() +
  guides(size = "none") +
  labs(
    title = "Telework is reaching its potential",
    subtitle = "Correlation between physical teleworkability and actual telework for ISCO 3-digit occupations",
    x = "Phyisical teleworkability index", y = "Actual telework"
  ) +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


# Telework frequencies by occupation, year, country -----------------------

occup_homework_freq <- LFS %>%
  filter(!isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(year, country, isco08_3d) %>%
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    homework_never = sum((homework == "Person never works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    homework_some  = sum((homework == "Person sometimes works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    homework_main  = sum((homework == "Person mainly works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco08_3d" = "code"))

occup_homework_freq %>%
  filter(year == 2019) %>%
  mutate(homework_any = 1 - homework_never) %>%
  group_by(isco08_3d) %>%
  summarise(
    homework_any_min = min(homework_any, na.rm = TRUE),
    homework_any_max = max(homework_any, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = isco08_3d, ymin = homework_any_min, ymax = homework_any_max)) +
  geom_errorbar() +
  coord_flip() +
  scale_x_reverse() +
  labs(
    x = "Share"
  )

plot_occup_homework_freq <- occup_homework_freq %>%
  filter(year == 2019) %>%
  group_by(isco08_3d, occupation, occup_group) %>%
  summarise(var_tw = var(homework_index, na.rm = TRUE) %>% round(3), .groups = "drop") %>%
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

ggplotly(plot_occup_homework_freq)




occup_homework_freq <- LFS %>%
  filter(!isco1d %in% "Non response", !is.na(isco1d)) %>%
  group_by(year, country, isco08_3d) %>%
  summarise(
    homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)),
    homework_never = sum((homework == "Person never works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    homework_some  = sum((homework == "Person sometimes works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    homework_main  = sum((homework == "Person mainly works at home") * coeffy) / sum(coeffy, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  left_join(labels_isco, by = c("isco08_3d" = "code"))

occup_homework_freq %>%
  filter(year == 2019) %>%
  mutate(homework_any = 1 - homework_never) %>%
  group_by(isco08_3d) %>%
  summarise(
    homework_any_min = min(homework_any, na.rm = TRUE),
    homework_any_max = max(homework_any, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = isco08_3d, ymin = homework_any_min, ymax = homework_any_max)) +
  geom_errorbar() +
  coord_flip() +
  scale_x_reverse() +
  labs(
    x = "Share"
  )

plot_occup_homework_freq <- occup_homework_freq %>%
  filter(year == 2019) %>%
  group_by(isco08_3d, occupation, occup_group) %>%
  summarise(var_tw = var(homework_index, na.rm = TRUE) %>% round(3), .groups = "drop") %>%
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

ggplotly(plot_occup_homework_freq)

