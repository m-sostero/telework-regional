library("tidyverse")

source("Code/0- Load common.R")


country_fe <- read_tsv("Tables/reg_regional_pooled_country.tsv") %>% 
  mutate(across(2:7, as.numeric))

names(country_fe) <- c("country", "1_est", "1_se", "2_est", "2_se", "3_est", "3_se")

country_fe %>% 
  arrange(desc(`1_est`)) %>% 
  mutate(country = fct_inorder(country)) %>% 
  ggplot(aes(x = country, y = `1_est`, ymin = `1_est` - 1.96*`1_se`, ymax = `1_est` + 1.96*`1_se`)) +
  geom_point() + geom_linerange() +
  scale_y_continuous(labels = percent_format()) +
  coord_flip()
  
