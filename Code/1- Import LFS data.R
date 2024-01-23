# Load common packages and labels ----
source("Code/0- Load common.R")

# Import LFS data produced by John in Code/Regional2023.do
LFS_raw <- read_dta("Data/LFSreg2018_22r.dta")
LFS_raw_old <- read_dta("Data/LFSreg2018_21finalr.dta")


# Clean LFS data ----------------------------------------------------------

# This block contains all the modifications to the LFS data,
# Which are then used for descriptive statistics in the following scripts
# Until the data is exported for regressions in Code/9- Combine data.R

LFS <- LFS_raw %>%
  # Recode specific values into missing values <NA>
  mutate(
    across(where(is.numeric), ~ na_if(., 999)),
    across(where(is.character), ~ na_if(., "99")),
    across(where(is.character), ~ na_if(., "999")),
    across(where(is.character), ~ na_if(., "")),
    # absreas = na_if(absreas, 99),
  ) %>%
  # Encode categorical variables from STATA as factors in R
  as_factor() %>%
  # Correct typo in stapro levels
  mutate(stapro = fct_recode(stapro, Employee = "£mployed")) %>%
  # Remove redundant observations created by previous merge in STATA
  filter(`_merge` != 2) %>%
  select(-`_merge`) %>% 
  # Include only employees and self-employed (exclude "Family workers" and a few NAs)
  filter(stapro %in% c("Employee", "Self-employed")) %>%
  mutate(
    # Encode homework index, using agreed-upon coefficients
    homework_index = case_when(
      homework == "Person mainly works at home" ~ 0.75, 
      homework == "Person sometimes works at home" ~ 0.25, 
      homework == "Person never works at home"  ~ 0 
    ),
    # Encode homework_any as a binary variable
    homework_any = if_else(homework_index > 0, 1L, 0L)
  ) %>% 
  relocate(homework_index, homework_any, .after = "homework") %>% 
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

#TODO: process again when John encodes REGION2DW and COUNTRYW
# %>% 
#   # code work_location as the region or country of work vs residence
#   mutate(
#     work_location = case_when(
#       # region of residence is the same as the region of residence (or includes it, in case the granularity of reg < regw)
#       str_detect(regw, paste0("^", reg)) ~ "Region of residence",
#       # Work in own MS, but different region than residence
#       ctryw == "Work in own MS" & !str_detect(regw, paste0("^", reg)) ~ "Other region in country of residence",
#       # Work in another MS
#       ctryw %in% c("Work in another EU MS or UK", "Work in EEA", "Work in other European country", "Work in ROW", "Work in another country") ~ "Other country",
#       # Missing region/country of residence, or no reply
#       is.na(regw) | is.na(ctryw) | ctryw == "Not stated" ~ "Not stated"
#     ) %>% factor(levels = c("Region of residence", "Other region in country of residence", "Other country", "Not stated"))
#   ) 

# Export LFS in fast binary format .feather for use in the following scripts
write_feather(LFS, "Data/LFS.feather")


# Clear LFS_raw from memory
rm(LFS_raw)
gc()
