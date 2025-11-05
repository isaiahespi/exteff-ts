# this script imports and processes a subset of variables from the ANES
# Cumulative Data File (1948-2020)

# The 2022-09-16 version of the ANES CDF is openly available to export at the following [link](https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/)

# Citation:
# American National Election Studies. 2022. ANES Time Series
# Cumulative Data File [dataset and documentation]. September 16,
# 2022 version. www.electionstudies.org

# set up ---------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load custom functions
source(here::here("utils", "funs.R"))

# load packages
# library(tidyverse)
# library(fipio)

# load data
anesCDF <- haven::read_sav("data-raw/anes/anes_timeseries_cdf_spss_20220916.sav")

# rename select columns and create cdf codebook ------------------------------

# optional
# this gets rid of the the annoying prefix embedded in the value labels of ANES
# variables, e.g., "1. Response"
# If no `.col` are specified, then applies to the whole data set
anesCDF <- anesCDF |> 
  labelled::update_value_labels_with(
    .cols = dplyr::where(labelled::is.labelled),
    .fn = \(x) stringr::str_remove(x, "^-*[0-9]+[\\.]\\s*")
    )

# prefix ANES CDF variable name to variable label. This will help ensure whether
# the renamed variable corresponds to the correct ANES CDF variable.
anesCDF <- anesCDF |> 
  labelled::update_variable_labels_with(.fn = \(x) paste(names(x), x, sep = ": "))

# save named character vector of new column names corresponding to ANES
# variables
new_var_names <- c(
  anes_version          = "Version",
  year                  = "VCF0004",
  case_id               = "VCF0006",
  panelcase_id          = "VCF0006a",
  weights9x             = "VCF0009x",
  weights9y             = "VCF0009y",
  weights9z             = "VCF0009z",
  weights10x            = "VCF0010x",
  weights10y            = "VCF0010y",
  weights10z            = "VCF0010z",
  weights11x            = "VCF0011x",
  weights11y            = "VCF0011y",
  weights11z            = "VCF0011z",
  weights_VCF9999       = "VCF9999",
  post_electionIW       = "VCF0013",
  pre_electionIW        = "VCF0014",
  cross_sec_comp        = "VCF0016",
  interview_mode        = "VCF0017",
  age                   = "VCF0101",
  age_group             = "VCF0102",
  cohort                = "VCF0103",
  gender                = "VCF0104",
  race_7cat             = "VCF0105a",
  race_4cat             = "VCF0105b",
  race_3cat             = "VCF0106",
  hisp_orgin_type       = "VCF0107",
  hisp_orgin            = "VCF0108",
  educ_4cat             = "VCF0110",
  census_region         = "VCF0112",
  political_south       = "VCF0113",
  income_group          = "VCF0114",
  hh_union_mbr          = "VCF0127",
  union_who             = "VCF0127b",
  relig_major           = "VCF0128",
  relig_7cat            = "VCF0128a",
  relig_8cat            = "VCF0128b",
  educ_6cat             = "VCF0140",
  educ_7cat             = "VCF0140a",
  birthplace            = "VCF0142",
  parents_native        = "VCF0143",
  partyid               = "VCF0301",
  partyid_3cat          = "VCF0303",
  interest_in_elections = "VCF0310",
  care_who_wins         = "VCF0311",
  care_who_wins_house   = "VCF0312",
  attention             = "VCF0313",
  pres_app_anes         = "VCF0450",
  pres_app_strngth      = "VCF0451",
  protest_approval      = "VCF0601",
  civildisobedience     = "VCF0602",
  demonstrations        = "VCF0603",
  trust1                = "VCF0604",
  trust2                = "VCF0605",
  trust3                = "VCF0606",
  trust4                = "VCF0608",
  nocare                = "VCF0609",
  nosay                 = "VCF0613",
  complex               = "VCF0614",
  govresp               = "VCF0624",
  exteff.indx           = "VCF0648",
  govresp.indx          = "VCF0649",
  trustgov.indx         = "VCF0656",
  turnout               = "VCF0702",
  regis_turnout         = "VCF0703",
  votechoice_candiates  = "VCF0704",
  votechoice_2party     = "VCF0704a",
  votechoice            = "VCF0705",
  voted                 = "VCF0706",
  vote_party_house      = "VCF0736",
  betteroff             = "VCF0880",
  betteroffa            = "VCF0880a",
  betteroff_next_year   = "VCF0881",
  congdist              = "VCF0900",
  stcd_fips             = "VCF0900b",
  stcd_abb              = "VCF0900c",
  st_fips               = "VCF0901a",
  st_abb                = "VCF0901b",
  pres_app_econ         = "VCF9009"
)


# use the named character vector to rename the ANES CDF variable column names
# this won't drop any of the unnamed variable columns
anesCDF <- anesCDF |> 
  dplyr::rename(dplyr::any_of(new_var_names))

# create codebook
anesCDF_codebook <- codebook(anesCDF)


# remove `new_var_names` from environment
rm(new_var_names)

# save --------------------------------------------------------------------

# save ANES CDF as .rds
readr::write_rds(anesCDF, file = "data/anes data/anes_cdf.rds")

# save codebook
readr::write_csv(
  anesCDF_codebook, 
  file = "resources/anes resources/anes cdf/anesCDF_codebook.csv"
  )

rm(list = ls())
