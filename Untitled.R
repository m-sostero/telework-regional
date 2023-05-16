# Homework index by urbrur ----
hw_urbrur <- LFS %>%  
  group_by(year, country, urbrur) %>% 
  summarise(
    homework_index = weighted.mean(homework_index, w = coeffy, na.rm = TRUE)
  ) 

hw_urbrur %>% 
  pivot_wider(names_from = urbrur, values_from = homework_index) %>% 
  view("Homework by urbrur")


plot_hw_urbrur <- hw_urbrur %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = country, color = country)) +
  geom_point() + geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  labs( title = "2019: Most countries have more homeworking in cities\n than in towns-sururbs, than in rural areas")

ggplotly(plot_hw_urbrur)

hw_urbrur %>% 
  filter(country %in% c("IE", "DE", "IT", "RO")) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  facet_grid(~ country) + 
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  geom_point() + geom_line() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

hw_urbrur %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = urbrur, y = homework_index, group = year, color = year)) +
  geom_point() + geom_line() +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  # scale_color_brewer(palette = "Blues") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

