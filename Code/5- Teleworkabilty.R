library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files


# Occupational teleworkability statistics ---------------------------------------

teleworkability <- read_dta("Data/Teleworkability indices.dta")

occupational_variables <- read_dta("Data/occup.dta") %>%
  as_factor() %>%
  arrange(country, year, isco08_3d)

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
