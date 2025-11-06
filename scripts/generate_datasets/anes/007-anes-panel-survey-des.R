# this script creates the survey design objects for use with the ANES
# 2016-2020-2024 panel data

# The ANES data are weighed to the sample, not the population. In order to make
# generalizations about the target population (e.g., national), the data must be
# weighed against the target population count.

# The 2024 ANES panel data has been merged to include variables from the 2020
# and 2016 ANES data corresponding to the same panel respondents. The panel data
# is then weighed against the target population described in the 2024 ANES
# documentation.

# The target population of the ANES 2016-2020-2024 panel sample consisted of
# U.S. citizens who were 18 or older and living in the 50 states or Washington,
# D.C., immediately before the 2016 presidential election.

# To weigh the panel sample against this target population, I first derive the
# population count from the Census 2016 ACS 1-year estimates.

# I then adjust the weight variable provided in the 2024 ANES (`V240106b`,
# renamed as `panel_wt`) against the population estimate derived from the 2016
# ACS 1-year Public Use Micro Data (PUMs).

# Specifically, I determine the proportion of the total weight for each
# individual weight and then multiply that proportion by the target population
# (e.g., (panel_weight_{i} / sum(panel_weight_{i})) * target_population).

# NOTE: Census API key required -------------------------------------------

# This script makes use of the `censusapi` R package which relies on the Census
# API to export data. A Census API Key is required.

# If you don't have a Census API key, get one from U.S. Census Bureau site
# https://api.census.gov/data/key_signup.html
# then store the Census API key in the R environment instead of directly within
# the code. Run the code below, replacing with your API key
# Sys.setenv(CENSUS_API_KEY = "YOUR_API_KEY_HERE")

# Once you have Census API key, restart R and run the following
# Sys.getenv("CENSUS_API_KEY")

# set up ------------------------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load some custom functions
source(here::here('utils', 'funs.R'))

# packages
# library(tidyverse)
# library(fs)
# library(censusapi)
# library(tidycensus)
# library(survey) # Analysis of Complex survey samples
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data

# import/load in ANES 2016-2020-2024 panel data
anes_panel <- readr::read_rds("data/anes data/anes_panel_2016_2020_2024.rds")

# This dataset includes survey data for the panel of respondents (cases) who
# participated in the post-election interviews across the previous three ANES
# surveys (n = 2,070). Every variable from each survey has been retained,
# although a smaller selection of variables been renamed for ease of use.

# import ANES Panel data codebook for reference
anes_panel_codebook <- readr::read_csv(
  file = "resources/anes resources/anes_panel_2016_2020_2024_codebook.csv")

# this codebook contains the variable names and labels for every variable
# included in the 2016, 2020, and 2024 ANES surveys. The variable labels have
# been prefixed with the original variable code from each ANES survey for ease
# of reference.

# derive target population for 2016 ------------------------------------------

# I am able to pull the 2016 ACS 1-year data using `censusapi` package

# censusapi::listCensusApis(vintage = 2016) |> 
#   dplyr::as_tibble() |> 
#   View()

# I rely on PUMs data (Public Use Micro data)
pum_2016vars <- censusapi::listCensusMetadata(name = "acs/acs1/pums", vintage = 2016, type = "variables")
pum_2016vars <- dplyr::as_tibble(pum_2016vars)

# identify PUMs variables of interest
# pum_2016vars |>
#   dplyr::filter(name %in% c("AGEP", "TYPE", "CIT", "PWGTP", "ST"))

# quick function to make it easier to get value labels from the census meta data
# using `censusapi` package
get_val_labels <- function(x){
  censusapi::listCensusMetadata(
  name = "acs/acs1/pums",
  vintage = 2016,
  type = "values",
  variable_name = x)
}

# a crude way to get the value labels and value code
pums_list <- c("AGEP", "TYPE", "CIT") |> purrr::map(get_val_labels)
names(pums_list) <- c("AGEP", "TYPE", "CIT")
purrr::map(pums_list, .f = \(x) tibble::tibble(x))

# AGEP: Age
# 00 = Under 1 year

# TYPE: Type of [housing] unit
# 1 = Housing unit                   
# 2 = Institutional group quarters   
# 3 = Noninstitutional group quarters

# CIT: Citizenship status
# 1 = Born in the U.S.                                                            
# 2 = Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas
# 3 = Born abroad of American parent(s)                                           
# 4 = U.S. citizen by naturalization                                              
# 5 = Not a citizen of the U.S.

# PWGTP: PUMS person weight
# ST: State of current residence

# obtain information from CPS needed for the survey design object
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict16.txt
acs1_2016 <- censusapi::getCensus(
  name = "acs/acs1/pums",
  vintage = 2016,
  region = "state", 
  vars = c("AGEP", "CIT", "TYPE", "PWGTP", "ST"),
  key = Sys.getenv("CENSUS_API_KEY")
)

acs1_2016 <- dplyr::as_tibble(acs1_2016)

# convert variables to numeric
# acs1_2016 <- acs1_2016 |> 
#   dplyr::mutate(dplyr::across(c(AGEP, CIT, TYPE, PWGTP, ST), ~as.numeric(.)))

acs1_2016 <- acs1_2016 |> 
  # add state abbreviation (`st_abbr`), state FIPS (`stfips`), and longstate name (`st`)
  dplyr::mutate(st_abbr = fipio::fips_abbr(state),
                st = fipio::fips_state(state),
                stfips = fipio::as_fips(st),
                state = as.numeric(ST),
                .before = AGEP, .keep = "unused")

# The target population of the ANES panel sample consists of the national
# population of U.S. citizens who were 18 or older and living in the 50 states
# or Washington, D.C., immediately before the 2016 presidential election.,
# excluding those living in institutional or group quarters.

# subset U.S. citizens, age 18 or older, non-institutional or group quarter
# 1 = Housing unit, 2 = institutional group quarters, 3 = non-institutional group quarters
acs1_2016 <- acs1_2016 |> 
  dplyr::filter(AGEP >= 18, CIT %in% c(1:4), TYPE == 1)

# get the non-institutionalized target population of U.S. citizens age 18 or older in 2016
targetpop2016 <- acs1_2016 |> dplyr::pull(PWGTP) |> sum()

# U.S. Citizens, 18 or older, residing in U.S. at time of 2016 election
# scales::comma(targetpop2016) 

# get the same by state
state_pops2016 <- acs1_2016 |> 
  dplyr::summarise(
    pop = sum(PWGTP), .by = c(st, st_abbr, state, stfips) 
  ) |> 
  dplyr::arrange(state) |> 
  dplyr::select(st, st_abbr, stfips, pop)

# return each state's population alongside proportion of national population 
# state_pops2016 |> 
#   dplyr::select(st, pop) |> 
#   dplyr::mutate(
#     pct = pop/sum(pop)*100,
#     pop = scales::comma(pop)
#     ) |> 
#   print(n = Inf)



# use `srvyr` R package to create survey design object --------------------

# the 2024 ANES panel weight sums to the panel sample, i.e., data is weighed to
# the sample and not the population.
# anes_panel |> 
#   dplyr::summarise(
#     sample_2024 = sum(wt_panel, na.rm = T)
#   )

# For post-election panel sample, use these PSU and stratum pairs
# wt_panel = V240106b, psu = V240106c, strata = V240106d

# create an adjusted weight variable. Multiply the proportion of the total
# weight of each individual case by the target population count derived from the
# ACS 1-year estimates
anes_panel <- anes_panel |> 
  dplyr::mutate(wt_panel_adj = (wt_panel/sum(wt_panel, na.rm = T)) * targetpop2016, .after = wt_panel)


# the adjusted weight variable will sum to the respective target population
# anes_panel |> 
#   dplyr::summarise(
#     population = scales::comma(sum(wt_panel_adj)),
#     unweighed_sample_n = scales::comma(sum(wt_panel)) 
#   )


# create the survey design object for 2016-2020-2024 ANES panel 
anes_panel_des <- anes_panel |> 
  srvyr::as_survey_design(
    weights = wt_panel_adj, 
    strata = strata,   # V240106d
    ids = psu,         # V240106c 
    nest = TRUE             # ensure clusters (PSU) are nested within strata
  )

# anes_panel_des

# save survey design objects ----------------------------------------------

# save as .rds
readr::write_rds(anes_panel_des, file = "data/anes data/anes_2016_2020_2024_panel_survey_des.rds")

# save state population estimates from ACS 1-year and PUMs Census data
readr::write_rds(state_pops2016, file = "data/population_states_2016_pums.rds")


rm(list = ls())
