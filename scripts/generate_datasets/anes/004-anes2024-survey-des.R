# this script creates the survey design objects for use with the ANES 2024 data

# The weights provided in the 2024 ANES are adjusted to the target population
# this is necessary due to the fact that the ANES data are weighed to the
# sample, not the population. In order to make generalizations about the target
# population (e.g., national), the data must be weighed against the target
# population count.

# There are different target populations that pertain to the different sample
# types. To weigh the full sample against national population, I use the
# population count from the Census ACS 1-year estimates to adjust the weight
# variable to the national population of U.S. citizens of age 18 years or older,
# excluding those living in institutional or group quarters.

# The ANES 2016-2020-2024 panel’s target population consisted of U.S. citizens
# who were 18 or older and living in the 50 states or Washington, D.C.,
# immediately before the 2016 presidential election. I employed the same method
# of using the ACS 1-year estimates to weigh the panel sample against the target
# population just mentioned.

# set up ------------------------------------------------------------------

# load some custom functions
source(here::here('utils', 'funs.R'))

# packages
library(tidyverse)
# library(fs)
# library(censusapi)
# library(tidycensus)
# library(survey) # Analysis of Complex survey samples
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data


# load ANES 2024 survey data subset 
anes2024 <- readr::read_rds(file = "data/anes data/anes2024_subset.rds")

# sometimes an issue, so zap embedded SPSS attribute formats
anes2024 <- haven::zap_formats(anes2024)

# load in ANES 2024 data subset codebook
anes2024_codebook <- readr::read_rds(file = "data/anes data/anes2024_subset_codebook.rds")


# benchmark ---------------------------------------------------------------

# "The target population for the in-person sample was 232.5 million U.S.
# citizens age 18 or older living in the 48 contiguous states of the U.S. or
# Washington, D.C., and the target population for the web sample was 234.1
# million U.S. citizens age 18 or older living in the 50 states or Washington,
# D.C. Both populations exclude those living in institutional or group
# quarters."

# The ANES 2016-2020-2024 panel’s target population consisted of U.S. citizens
# who were 18 or older and living in the 50 states or Washington, D.C., immediately
# before the 2016 presidential election

# People who authored the complex surveys book posted a blog post for ANES 2024 data
# https://rworks.dev/posts/anes2024/

# The 2024 ACS 1-year pums data has not been released yet, unfortunately. Though
# not really necessary as the 2023 ACS 1-year estimates work just as well.
# 2023 ACS 1-year estimates since no 2024 data in `pums_variables`

acs1_2023vars <- tidycensus::load_variables(year = 2023, dataset = "acs1/subject", cache = TRUE)
# acs1_2024vars <- tidycensus::load_variables(year = 2024, "acs1/subject", cache = TRUE)

# import 2023 ACS 1-year data population estimates
acs12023cit18 <- tidycensus::get_acs(
    geography = "state",
    variable = "S2901_C01_001",
    year = 2023,
    survey = "acs1"
  )

# the ACS1 variable from 2023 "S2901_C01_001" refers to the subject table:
# S2901 | Citizen voting-age population by selected characteristics
# This is the same for the 2024 ACS 1-year estimates

# Subject Tables provide a span of information on a particular ACS subject
# presented in the format of both estimates and percentages.
# [source](https://www.census.gov/data/developers/data-sets/acs-1year.html)

# View the var_code in the pums_variables for specific year and survey
# tidycensus::pums_variables |> 
#   dplyr::as_tibble() |> 
#   dplyr::filter(year == 2023, survey == "acs1") |> 
#   View()


# save pums_variables for 2023 ACS 1-year data
pumvars_2023 <- tidycensus::pums_variables |> 
  dplyr::as_tibble() |> 
  dplyr::filter(year == 2023, survey == "acs1")

# look at the `var_code`. We can see the value labels and value codes
pumvars_2023 |> 
  dplyr::filter(var_code %in% c("AGEP", "TYPEHUGQ", "CIT")) |> 
  dplyr::select(var_code, var_label, val_min, val_max, val_label)


# function to return estimate of target population for particular state
get_citpop18grp <- function(state){
  tidycensus::get_pums(
    variables = c("AGEP", "TYPEHUGQ"),
    state = state,
    year = 2023,
    survey = "acs1",
    variables_filter = list(
                       TYPEHUGQ = 2:3,
                       CIT = 1:4,
                       AGEP = (18:200)
                       )
    ) |> 
    dplyr::summarise(estimate = sum(PWGTP), .by = STATE)
}

# save population of U.S. citizens age 18 or older and in group quarters for
# each state as a list
# use `purrr::map` to get target population for every state including D.C.
# `state.abb` is already in base R, but excludes DC
get_citpop18grp_l <- c(state.abb, "DC") |> purrr::map(get_citpop18grp)

# row bind the list and rename the `estimate` to `est_gq` This gives me the
# population estimate of U.S. citizens 18 or older residing in group quarters
# for each state
# this estimate will later be subtracted from the target population gathered
citpop18grp <- get_citpop18grp_l |> 
  purrr::list_rbind() |> 
  dplyr::rename(est_gq = estimate)

# join data sets together
# create variable that shows pop estimate of U.S. citizens 18 or older not
# living in group quarters or institutionalized
# `est_scope` is created by subtracting the estimated population in group
# quarters from the total estimated population of U.S. citizens age 18 or older
# in 2023
state_pops <- acs12023cit18 |> 
  dplyr::select(GEOID, NAME, estimate_n = estimate, moe) |> 
  dplyr::full_join(citpop18grp, by = c("GEOID" = "STATE")) |> 
  dplyr::filter(NAME != "Puerto Rico") |> 
  dplyr::mutate(est_scope = estimate_n - est_gq)

rm(citpop18grp, get_citpop18grp_l, acs12023cit18)

# compute in-person target population (i.e., excluding Alaska and Hawaii)
# this is the target population for the in-person sample specifically
# any analysis that uses only the in-person sample should adjust weights based
# on this population
targetpop_inperson <- state_pops |> 
  dplyr::filter(!GEOID %in% c("02", "15")) |>
  dplyr::pull(est_scope) |> 
  sum()
  

# Target population -- estimate of U.S. citizens 18 or older not
# living in group quarters or institutionalized
# this is the target population for the web sample, which simply includes Alaska
# and Hawaii
targetpop <- state_pops |> 
  dplyr::pull(est_scope) |> 
  sum()

# target population for 2016 ----------------------------------------------

# one more target population must gathered; the target population of the ANES
# 2016-2020-2024 panel. This target population consisted of U.S. citizens who
# were 18 or older and living in the 50 states or D.C. immediately before the
# 2016 U.S. general election.

# Unfortunately, no pums data available for years prior to 2017 in `tidycensus`
# I am able to pull the data using `censusapi` package, however
# censusapi::listCensusApis(vintage = 2016) |> 
#   dplyr::as_tibble() |> 
#   View()

# I rely on the same PUMs data (Public Use Micro data)
pum_2016vars <- censusapi::listCensusMetadata(name = "acs/acs1/pums", vintage = 2016, type = "variables")
pum_2016vars <- dplyr::as_tibble(pum_2016vars)


# I can't look at the `var_code` the same way I did with `tidycensus. 
# pum_2016vars |> 
#   dplyr::filter(name %in% c("AGEP", "TYPE", "CIT"))

# instead, I do it the `censusapi` way
# quick function to make it easier
# get_val_labels <- function(x){
#   censusapi::listCensusMetadata(
#   name = "acs/acs1/pums", 
#   vintage = 2016, 
#   type = "values",
#   variable_name = x)
# }

# a crude way to get the value labels and value code
# pums_list <- c("AGEP", "TYPE", "CIT") |> purrr::map(get_val_labels)


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
acs1_2016 <- acs1_2016 |> 
  dplyr::mutate(dplyr::across(c(AGEP, CIT, TYPE, PWGTP, ST), ~as.numeric(.)))

acs1_2016 <- acs1_2016 |> 
  # add state abbreviation (`st_abbr`), state FIPS (`stfips`), and longstate name (`st`)
  dplyr::mutate(st_abbr = fipio::fips_abbr(state),
                st = fipio::fips_state(state),
                stfips = fipio::as_fips(st),
                state = as.numeric(ST),
                .before = AGEP, .keep = "unused")


# subset U.S. citizens, age 18 or older, non-institutional or group quarter
# 1 = Housing unit, 2 = institutional group quarters, 3 = non-institutional group quarters
acs1_2016 <- acs1_2016 |> 
  dplyr::filter(AGEP >= 18, CIT %in% c(1:4), TYPE == 1)


# get the non-institutionalized target population of U.S. citizens age 18 or older in 2016
targetpop2016 <- acs1_2016 |> dplyr::pull(PWGTP) |> sum()


# get the same by state
state_pops2016 <- acs1_2016 |> 
  dplyr::summarise(
    tgtpop = sum(PWGTP), .by = c(state, st_abbr) 
  ) |> 
  dplyr::arrange(state)


# view the different target populations
scales::comma(targetpop)
scales::comma(targetpop_inperson)
scales::comma(targetpop2016)


# Quick note on Weights in ANES -------------------------------------------

# Note: ANES weights add up to the sample size, not the population size. The
# code below confirms this
# The results match the N reported in the ANES 2024 documentation.

# anes2024 |>
#   # dplyr::filter(wt_full > 0) |>
#   dplyr::summarise(dplyr::across(
#     c(
#       wt_ftf,    # post-election, fresh in-person alone
#       wt_web,    # post, fresh web + PAPI
#       wt_fresh,  # Post fresh sample (fresh in-person + fresh web + PAPI)
#       wt_panel,  # post, panel
#       wt_full    # Post, full sample (fresh in-person + fresh web + panel) + PAPI
#     ),
#     ~ sum(., na.rm = T)
#   ), .by = sample_type.fct)





# apply weights to ANES 2024 data -----------------------------------------

# So since there are essentially two separate "fresh" samples drawn from the same population, I don't have just one weight variable, but at least two out of the 16 separate weights divided among the pre- and post-election surveys. There is the post-election fresh in-person sample and a post-election fresh web sample, both from approximately the same population. For the in-person sample, the ANES excludes Alaska and Hawaii (probably due to expense), so the target population for the in-person sample is less than the target population of the web sample.

# There are at least 16 weight variables I can select from, however at most I will only need three. # I include the weight that corresponds to the post-election full sample (fresh in-person + fresh web + panel) + PAPI (V240107b). This allows me to weigh the data against the full target population, including Alaska and Hawaii (rather than weighed to the sample).

# I'm also interested specifically in the ANES 2016-2020-2024 panel data, and so include the post-election weights for the panel alone (V240106b) and adjust the panel weight according to the target population for the sample pertaining to panel respondents.

# Since the most pertinent inquiry I have considers the 2016-2024 panel data, then I'll focus on that group in the analysis. I may have to make a separate weighted data set subset to only panel respondents. 

# anes2024 |>
#   dplyr::select(dplyr::contains("wt_"),
#                 dplyr::contains("stratum"),
#                 dplyr::contains("psu")) |>
#   var_label_tab()

# For post-election full sample (fresh in-person + fresh web + panel) + PAPI,
# wt_full = V240107b, psu = V240107c, strata = V240107d

# For post-election panel sample, use these PSU and stratum pairs
# wt_panel = V240106b, psu = V240106c, strata = V240106d

# I create two adjusted weight variables using the target population.
# adjust the weight variable (`V240106b`, renamed as `panel_wt`) using the
# population of interest just calculated from the CPS data.
# Determine the proportion of the total weight for each individual weight
# (wt/sum(wt)), and then multiply that proportion by the calculation population
# of interest `targetpop`.

anes2024adjwt <- anes2024 |>
  dplyr::mutate(wtadj_panel = (wt_panel/sum(wt_panel, na.rm = T)) * targetpop2016,
                wtadj_full = (wt_full/sum(wt_full)) * targetpop)


# now the adjusted weights sum to the respective target populations
anes2024adjwt |> 
  dplyr::summarise(
    targetpop_sum = scales::comma(sum(wtadj_full)),
    targetpop_panel = scales::comma(sum(wtadj_panel, na.rm = T))
  )


# create the survey design object for ANES 2024
anes2024des <- anes2024adjwt |> 
  srvyr::as_survey_design(
    weights = wtadj_full, # (wtadj_full/sum(wtadj_full)) * targetpop
    strata = stratum_full, # V240107d
    ids = psu_full,         # V240107c 
    nest = TRUE           # ensure clusters (PSU) are nested within strata
  )

anes2024des


# save survey design objects ----------------------------------------------

# save as .rds
readr::write_rds(anes2024des, file = "data/anes data/anes2024des.rds")

# save state population estimates from ACS 1-year and PUMs Census data
readr::write_rds(state_pops, file = "data/population_states_2023_acs1.rds")
readr::write_rds(state_pops2016, file = "data/population_states_2016_pums.rds")


rm(list = ls())




