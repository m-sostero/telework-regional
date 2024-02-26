# Load common packages and labels ----
source("Code/0- Load common.R")


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

# Internet speed by NUTS x DEGURBA (see 7- Regional connectivity.R)
# Currently includes only data for 2019 (NUTS 2016) and 2022 (NUTS 2021):
# - expand and impute to intermediate years
# Always reported at NUTS-2: aggregate when necessary for LFS microdata 

speed_nuts_degurba <- read_rds("Data/speed_nuts_degurba.rds")

# Expand NUTS x DEGURBA for all years 2018:2022, impute missing values
speed_nuts_degurba_years <- speed_nuts_degurba %>% 
  complete(NUTS_ID, degurba, year = 2018:2022) %>% 
  # Fill missing observations with last known value. 2018 uses 2019 data
  group_by(NUTS_ID) %>% 
  spread(degurba, avg_speed_mbps) %>% 
  fill(Cities, `Towns and suburbs`, `Rural areas`, .direction = "downup") %>% 
  ungroup()
  
# Check which regions are reported at aggregate NUTS<2 in LFS
LFS %>%
  distinct(reg) %>%
  filter(str_length(reg) < 4)
# AT and DE (NUTS-1) and NL (NUTS-0)

# Aggregate internet speeds for those countries as appropriate
speed_reg_degurba_years <- speed_nuts_degurba_years %>% 
  # re-code NUTS -> reg, with AT, DE at 1-digit, and NL at 0 digits
  mutate(
    reg = NUTS_ID,
    reg = str_replace_all(reg, "^((AT|DE).).", "\\1"), # Truncate AT and DE to 1-digit
    reg = str_replace_all(reg, "^(NL)..", "NL") # Truncate NL to 0-digit
  ) %>% 
  # Average the internet speeds at reg X DEGURBA
  select(-NUTS_ID) %>% 
  pivot_longer(cols = Cities:`Rural areas`, names_to = "degurba", values_to = "internet_speed") %>% 
  group_by(reg, year, degurba) %>% 
  summarise(internet_speed = mean(internet_speed, na.rm = TRUE), .groups = "drop")

# Are any LFS regions x years missing? 
anti_join(LFS %>% distinct(year, reg), speed_reg_degurba_years, by = c("reg", "year"))
# Should be empty

# Create LFS table for regression ---------------------------

LFS_regression <- LFS %>% 
  # Exclude BG, MT, SI, because they report ISCO at 1-digit level
  filter(!country %in% c("BG", "MT", "SI")) %>% 
  # Add telework values for occupations
  inner_join(occupational_variables, by = "isco_3d_code") %>% 
  # Add regional broadband statistics, at the most precise NUTS available,
  left_join(speed_reg_degurba_years, by = c("year", "reg", "degurba")) %>% 
  # Now that the ISCO matching with strings is complete, remove isco_3d_code (values identical to isco08_3d, which is numeric)
  select(-isco_3d_code) %>% 
  # Encode degurba (was converted to string at some point)
  mutate(degurba = factor(degurba))

# Add variable labels (for exporting later with Stata)
var_label(LFS_regression) <- list(
  urbrur = "Regional typology (NUTS)",
  degurba = "Degree of urbanisation (LAU)",
  work_location = "Place of work",
  ftpt = "Full- or part-time work",
  internet_speed = "Internet speed (NUTS x degurba)",
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



# Deprecated: old broadband data match ------------------------------------------------

# Regional-level access to broadband internet
nuts_broadband <- read_rds("Data/nuts_broadband.rds") %>%
  # Sort NUTS code from longest (more digits) to shortest, 
  # to allow for most precise regional-level matching available later
  mutate(NUTS_length = str_length(NUTS_ID)) %>%
  arrange(year, desc(NUTS_length), NUTS_ID) %>%
  select(-NUTS_length)

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

