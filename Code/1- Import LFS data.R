library("tidyverse") # collection of packages for working with data

library("haven") # read/write STATA dta data
library("arrow") # read/write arrow .feather files

# Import LFS data produced by Code/Regional2023.do
LFS_raw <- read_dta("../Data/LFSreg2018_21finalr.dta")

LFS <- LFS_raw %>%
  # Recode specific values into missing values <NA>
  mutate(
    across(where(is.numeric), ~ na_if(., 999)),
    across(where(is.character), ~ na_if(., "99")),
    across(where(is.character), ~ na_if(., "999")),
    absreas = na_if(absreas, 99)
  ) %>%
  # Encode categorical variables from STATA as factors in R
  as_factor() %>%
  # Correct typo in stapro levels
  mutate(stapro = fct_recode(stapro, Employed = "£mployed")) %>%
  # Remove redundant observations created by previous merge in STATA, and delete `merge` variable itself
  filter(`_merge` != 2) %>%
  filter(ilostat == "Employed") %>%
  select(-`_merge`, -ilostat)

# Export LFS in fast binary format .feather
write_feather(LFS_raw, "../Data/lfs.feather")

rm(LFS_raw)
gc()
