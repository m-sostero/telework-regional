# Load common packages and labels ----
source("Code/0- Load packages.R")

library("fixest")

# Load data ---------------------------------------------------------------

# Labour Force Survey microdata, cleaned, with work location
LFS <- read_feather("Data/LFS.feather")

# Teleworkability values for ISCO occupation. 
# The 3-digit values come from Sostero et al. 
# Values for 2-digit, 1-digit codes and some 3-digit codes that were not included have been imputed by Enrique 
occupational_variables <- read_dta("Data/occup.dta") %>%
  as_factor() %>%
  distinct(isco08_3d, physicalinteraction, socialinteraction) %>% 
  arrange(isco08_3d, physicalinteraction, socialinteraction) %>% 
  distinct(isco08_3d, .keep_all = TRUE) %>% 
  mutate(isco_3d_code = as.character(isco08_3d), isco08_3d = NULL) %>% 
  select(isco_3d_code, everything())

# Which ISCO 3-digit occupations in LFS don't have telework values?
LFS %>%
  count(isco_3d_code) %>% 
  anti_join(occupational_variables, by = "isco_3d_code") 
# Mostly armed forces, plus rare elementary occupation (950); overall, ok

# Create LFS table for regression ---------------------------

LFS_regression <- LFS %>% 
  # Exclude BG, MT, SI, because they report ISCO at 1-digit level
  filter(!country %in% c("BG", "MT", "SI")) %>% 
  # Add telework values for occupations
  left_join(occupational_variables, by = "isco_3d_code") %>% 
  # Define homework_any for people working from home at least some of the time
  mutate(homework_any = if_else(homework_index > 0, 1, 0)) %>%
  relocate(homework_any, .after = homework_index)

# Export in R .feather format
write_feather(LFS_regression, "Data/LFS_regression.feather")

# Export in Stata .dta format
write_dta(LFS_regression, "Data/LFS_regression.dta")


# Regressions -------------------------------------------------------------

feols(homework_index ~ physicalinteraction, data =  LFS_regression)

feols(homework_index ~ physicalinteraction | csw0(country, year), data =  LFS_regression) %>% etable()

feols(homework_any ~ physicalinteraction | csw0(country, year), data =  LFS_regression) %>% etable()






