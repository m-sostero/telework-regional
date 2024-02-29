library("tidyverse")

source("Code/0- Load common.R")


country_fe <- read_tsv("Tables/reg_regional_pooled_country.tsv") %>% 
  mutate(across(2:7, as.numeric))

names(country_fe) <- c("country", "1_est", "1_se", "2_est", "2_se", "3_est", "3_se")

labels_country_stata <- tribble(
  ~country, ~country_code,
  "1.country",  "AT",
  "2.country",  "BE",
  "3.country",  "BG",
  "4.country",  "CY",
  "5.country",  "CZ",
  "6.country",  "DE",
  "7.country",  "DK",
  "8.country",  "EE",
  "9.country",  "EL",
  "10.country", "ES",
  "11.country", "FI",
  "12.country", "FR",
  "13.country", "HR",
  "14.country", "HU",
  "15.country", "IE",
  "16.country", "IT",
  "17.country", "LT",
  "18.country", "LU",
  "19.country", "LV",
  "20.country", "MT",
  "21.country", "NL",
  "22.country", "PL",
  "23.country", "PT",
  "24.country", "RO",
  "25.country", "SE",
  "26.country", "SI",
  "27.country", "SK"
)

country_fe %>%
  left_join(labels_country_stata, by = "country") %>% 
  left_join(labels_country, by = "country_code") %>% 
  arrange(desc(`2_est`)) %>% 
  mutate(country_name = fct_inorder(country_name)) %>% 
  ggplot(aes(y = country_name, x = `2_est`, xmin = `2_est` - 1.96*`2_se`, xmax = `2_est` + 1.96*`2_se`)) +
  geom_point() + geom_linerange() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  annotate("text", x = -0.06, y = 3, label = "Telework less common\nthan in Germany", colour = "grey40") +
  annotate("text", x =  0.06, y = 3, label = "Telework more common\nthan in Germany", colour = "grey40") +
  labs(
    y = "Country",
    x = "Country fixed effect and with 95% confidence interval\n(relative to Germany)"
  )

ggsave("Figures/reg_country_coefficient.png", width = 7, height = 5, bg = "white")
ggsave("Figures/reg_country_coefficient.pdf", width = 7, height = 5)
ggsave("Figures/reg_country_coefficient.svg", width = 7, height = 5)



country_fe %>%
  pivot_longer(-country) %>%
  separate(name, sep = "_", into = c("model", "coeff")) %>%
  spread(coeff, value) %>% 
  left_join(labels_country_stata, by = "country") %>% 
  left_join(labels_country, by = "country_code") %>% 
  # mutate(country_name = fct_inorder(country_name)) %>% 
  ggplot(aes(y = country_name, x = est, xmin = est - 1.96*se, xmax = est + 1.96*se, colour = model)) +
  geom_point() + geom_linerange(position = position_dodge2(width = 0.8)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  annotate("text", x = -0.06, y = 3, label = "Telework less common\nthan in Germany", colour = "grey40") +
  annotate("text", x =  0.06, y = 3, label = "Telework more common\nthan in Germany", colour = "grey40") +
  labs(
    y = "Country",
    x = "Country fixed effect and with 95% confidence interval\n(relative to Germany)"
  )
