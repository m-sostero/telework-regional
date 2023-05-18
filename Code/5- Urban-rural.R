library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

library("janitor") # convenience functions to clean and adorn summary tables

library("geofacet") # facet maps in position of EU geography

library("plotly") # Interative charts

library("RColorBrewer") # Color gradients for plots
library("scales") # graph scales (percent, comma, currency)
theme_set(theme_minimal()) # set minimalist theme as default for ggplot


# Load LFS, regional telework data, and NUTS maps -------------------------

LFS <- read_feather("Data/LFS.feather") 

labels_country <- geofacet::eu_grid1 %>% select(country_code = code, country_name = name) %>% as_tibble()

# Edit EU grid for cartogram to remove UK
eu_grid <- eu_grid1 %>% filter(code != "UK")



# Homework index by degurba ----

hw_degurba <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  group_by(year, country, degurba) %>% 
  summarise(homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)), .groups = "drop") %>%
  left_join(labels_country, by = c("country" = "country_code"))

hw_degurba %>% 
  pivot_wider(names_from = degurba, values_from = homework_index) %>% 
  mutate(across(`Rural areas`:Cities, ~ round(.*100, 1))) %>%
  view("Homework by degurba, weighted")


plot_hw_degurba <- hw_degurba %>% 
  filter(year == 2019) %>%
  ggplot(aes(x = degurba, y = homework_index, group = country_name, color = country_name)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs(title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_degurba)

hw_degurba %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = eu_grid, scales = "free_y") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "Telework has become more common, especially in cities, since 2020",
    subtitle = "Teleworking frequency by degree of urbanisation of respondents' residence, over the years",
    x = "Degree of urbanisation",
    y = "Teleworking frequency index\n(different scales)",
    source = "EU Labour Force Survey,\n own elaboration"
  )

hw_degurba %>% 
  filter(country %in% c("DE", "FR", "IT", "ES", "IE", "NL", "RO", "SE")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = degurba, y = homework_index, group = year, color = year)) +
  facet_wrap(country_name ~ ., nrow = 2) + 
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "Signigicant cross-country differences remain in rates of telework",
    subtitle = "Selected countries",
    x = "Degree of urbanisation",
    y = "Teleworking frequency index",
    source = "EU Labour Force Survey,\n own elaboration"
  )

LFS_employed <- LFS %>%
  group_by(year, country) %>% 
  summarise(total_pop = sum(coeffy, na.rm = T), .groups = "drop") 

hw_degurba_freq <- LFS %>%
  mutate(degurba = fct_rev(degurba)) %>% 
  group_by(year, country, degurba, homework) %>% 
  summarise(total = sum(coeffy, na.rm = TRUE), .groups = "drop") %>%
  left_join(LFS_employed, by = c("year", "country")) %>% 
  mutate(
    share = total / total_pop,
    homework = fct_recode(homework, never = "Person never works at home", sometimes = "Person sometimes works at home", usually = "Person mainly works at home") %>% fct_rev()
  ) %>% 
  left_join(labels_country, by = c("country" = "country_code"))

hw_degurba_freq %>%
  filter(country == "IE", !is.na(homework)) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = share, group = homework, fill = homework)) +
  geom_col(position = "stack") +
  facet_wrap(~ degurba) +
  scale_fill_brewer("Work at home", palette = "PuBu", direction = -1) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Ireland: Work from home has increased much more than the fall of residents in cities",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Share of people employed"
  )

hw_degurba_freq %>% 
  filter(country == "IE", !is.na(homework)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) +
  geom_col(position = "stack") +
  facet_wrap(~ degurba) +
  scale_fill_brewer("Work at home", palette = "PuBu", direction = -1) +
  scale_y_continuous(labels = comma_format(accuracy = 1, scale = 1000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Ireland: Work from home has increased much more than the fall of residents in cities",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Number of people employed"
  )

hw_degurba_freq %>%
  filter(country == "IE", !is.na(homework)) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = homework, y = share, group = year, fill = year)) +
  geom_col(position = "dodge") +
  facet_wrap(~ degurba) +
  # scale_fill_viridis_d() +
  scale_fill_brewer(palette = "BuGn") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "top") +
  labs(
    title = "Ireland: Work from home has increased much more than the fall of residents in cities",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Share of people employed"
  )


hw_degurba_freq %>%
  filter(country == "ES", !is.na(homework)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = total, group = homework, fill = homework)) + geom_col(position = "stack") +
  facet_wrap(~ degurba) +
  scale_fill_brewer("People working\nform home", palette = "PuBu", direction = -1) +
  scale_y_continuous(labels = comma_format(accuracy = 1, scale = 1000)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Spain",
    subtitle = "Number of employed people by degree of urbanisation and frequency of work from home",
    y = "Number of people"
  )


# Homework index by urbrur ----
hw_urbrur <- LFS %>%  
  mutate(urbrur = replace_na(urbrur, "Regions undifferentiated") %>% fct_rev()) %>% 
  group_by(year, country, urbrur) %>% 
  summarise(homework_index = (sum(homework_index*coeffy, na.rm = TRUE)/sum(coeffy, na.rm = TRUE)), .groups = "drop") %>%
  left_join(labels_country, by = c("country" = "country_code"))

hw_urbrur %>%
  pivot_wider(names_from = urbrur, values_from = homework_index) %>% 
  select(year, country, country_name, levels(hw_urbrur$urbrur)) %>% 
  view("Homework by urbrur")

plot_hw_urbrur <- hw_urbrur %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = country, color = country)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs(title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_urbrur)

hw_urbrur %>%
  filter(country %in% c("IE", "DE", "IT", "RO")) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  facet_grid(~country) +
  scale_color_viridis_d() +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

hw_urbrur %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  geom_point() +
  geom_line() +
  facet_geo(~ country, grid = eu_grid, scales = "free_y") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#TODO: zoom in in countries with both capital and mainly urban to show "capital-region premium"


