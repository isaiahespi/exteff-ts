# construct cost of voting index data set from data downloaded directly from the
# cost of voting index site page (https://costofvotingindex.com/data).

# Cost of voting index is composed of 10 issue area components (as of 2024)
# 1 registration deadline (ratio-level)
# 2 voter registration restrictions (9-item additive sub-index)
# 3 Registration drive restrictions (4-item additive sub-index)
# 4 Pre-registration laws (7-item ordered scale)
# 5 automatic voter registration (4-item ordered scale)
# 6 voting inconvenience (12-item additive sub-index)
# 7 voter ID laws (5-item ordered scale)
# 8 Poll hours (ratio-level)
# 9 Early voting days (ratio-level)
# 10 Absentee voting (10-item additive sub-index)


# set up ------------------------------------------------------------------

# packages
library(tidyverse)


# use `readxl` package to import covi values from 1996-2024
covi <- readxl::read_xlsx(path = "data-raw/covi/COVI Values 1996-2024 website.xlsx") |> 
  dplyr::mutate(year = as.integer(year)) |> 
  dplyr::relocate(year, .before = statenu)

covi <- covi |> 
  dplyr::rename(st_abb = state) |>
  dplyr::mutate(st_fips = fipio::as_fips(st_abb), .after = st_abb)

covi <- covi |> 
  dplyr::mutate(st = fipio::fips_state(st_fips), .before = st_abb)

