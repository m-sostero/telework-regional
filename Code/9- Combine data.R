# Load common packages and labels ----
source("Code/0- Load packages.R")

library("fuzzyjoin") # Regex-based matching, used for NUTS codes
library("labelled") # work with variable labels (Stata)

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

# Which ISCO 3-digit occupations in LFS don't have teleworkability values?
LFS %>%
  count(isco_3d_code) %>% 
  anti_join(occupational_variables, by = "isco_3d_code") 
# Mostly armed forces, plus rare elementary occupation (950) as expected.
# These will be excluded by the inner_join!


# Regional-level access to broadband internet
nuts_broadband <- read_rds("Data/nuts_broadband.rds") %>%
  # Sort NUTS code from longest (more digits) to shortest, to allow for most precise regional-level matching available later
  mutate(NUTS_length = str_length(NUTS_ID)) %>%
  arrange(year, desc(NUTS_length), NUTS_ID) %>%
  select(-NUTS_length)

# Make list of NUTS x year regions, as they appear in the LFS (different NUTS levels by country)
LFS_regions <- LFS %>% count(year, reg) %>% select(-n)

# Match LFS NUTS regions with the regional broadband statistics, at highest granularity available
LFS_regions_broadband <- regex_left_join(LFS_regions, nuts_broadband, by = c("reg" = "NUTS_ID", "year" = "year")) %>% 
  distinct(year.x, reg, .keep_all = TRUE) %>%  
  select(year = year.x, reg, broadband_shh)

# Check: Is any LFS NUTS region excluded? No
regex_anti_join(LFS_regions, nuts_broadband, by = c("reg" = "NUTS_ID", "year" = "year")) 

# Diagnostic: Show which LFS NUTS are matched at a different (more aggregate) level, for lack of more granular data
regex_left_join(LFS_regions, nuts_broadband, by = c("reg" = "NUTS_ID", "year" = "year")) %>% 
  distinct(year.x, reg, .keep_all = TRUE) %>% 
  distinct(year.x, reg, .keep_all = TRUE) %>% filter(reg != NUTS_ID) %>% 
  view("Approximate NUTS matching")
# This concerns mostly EL and PL (and FI20), where LFS reg is at NUTS-2, but broadband data is at NUTS-1 


# Create LFS table for regression ---------------------------

LFS_regression <- LFS %>% 
  # Exclude BG, MT, SI, because they report ISCO at 1-digit level
  filter(!country %in% c("BG", "MT", "SI")) %>% 
  # Add telework values for occupations
  inner_join(occupational_variables, by = "isco_3d_code") %>% 
  # Add regional broadband statistics, at the most precise NUTS available,
  left_join(LFS_regions_broadband, by = c("year", "reg")) %>% 
  # Define homework_any for people working from home at least some of the time
  mutate(homework_any = if_else(homework_index > 0, 1, 0)) %>%
  relocate(homework_any, .after = homework_index) %>% 
  # Now that the ISCO matching with strings is complete, remove isco_3d_code (values identical to isco08_3d, which is numeric)
  select(-isco_3d_code)

# Add variable labels (for exporting later with Stata)
var_label(LFS_regression) <- list(
  urbrur = "Regional typology (NUTS)",
  degurba = "Degree of urbanisation (LAU)",
  work_location = "Place of work",
  ftpt = "Full- or part-time work",
  broadband_shh = "Regional broadband coverage",
  physicalinteraction = "TWY: physical",
  socialinteraction = "TWY: social",
  homework_index = "Homeworking frequency",
  homework_any = "Homeworking, binary"
)

var_label(LFS_regression)

# Export datasets for regression ------------------------------------------

# Export in R .feather format
write_feather(LFS_regression, "Data/LFS_regression.feather")

# Export in Stata .dta format
write_dta(LFS_regression, "Data/LFS_regression.dta")
