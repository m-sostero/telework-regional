# Load common packages
source("Code/0- Load packages.R")


# Import LFS data produced by Code/Regional2023.do
LFS_raw <- read_dta("Data/LFSreg2018_21finalr.dta")

LFS <- LFS_raw %>%
  # Recode specific values into missing values <NA>
  mutate(
    across(where(is.numeric), ~ na_if(., 999)),
    across(where(is.character), ~ na_if(., "99")),
    across(where(is.character), ~ na_if(., "999")),
    across(where(is.character), ~ na_if(., "")),
    absreas = na_if(absreas, 99)
  ) %>%
  # Encode categorical variables from STATA as factors in R
  as_factor() %>%
  # Correct typo in stapro levels
  mutate(stapro = fct_recode(stapro, Employee = "£mployed")) %>%
  # Remove redundant observations created by previous merge in STATA
  filter(`_merge` != 2) %>%
  # Include only employees and self-employed (exclude "Family workers" and a few NAs)
  filter(stapro %in% c("Employee", "Self-employed")) %>%
  select(-`_merge`) %>% 
  # Encode homework index, using agreed-upon coefficients
  mutate(
    homework_index = case_when(
      homework == "Person mainly works at home" ~ 0.75, 
      homework == "Person sometimes works at home" ~ 0.25, 
      homework == "Person never works at home"  ~ 0 
    )
  ) %>% 
  relocate(homework_index, .after = "homework") %>% 
  # Create isco_3d_code, a string version of isco08_3d, left-padded with zeroes to reach 3-digit
  # This fixes problem with armed forces occupations, which start with 0
  mutate(isco_3d_code = str_pad(isco08_3d, width = 3, side = "left", pad = "0")) %>% 
  relocate(isco_3d_code, .after = "isco08_3d") %>% 
  # Fix NUTS codes, some of which end with 0 where they shouldn't
  mutate(
    # Strip last 0 from AT and DE, which are not in the NUTS specs
    reg = str_replace_all(reg, "(?<=AT\\d)0", ""),
    reg = str_replace_all(reg, "(?<=DE.)0", ""),
    # NL00 is just "NL"
    reg = str_replace_all(reg, "NL00", "NL")
  )
  

# Export LFS in fast binary format .feather
write_feather(LFS, "/Data/LFS.feather")

rm(LFS_raw)
gc()
