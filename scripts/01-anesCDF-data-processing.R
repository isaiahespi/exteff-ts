# using the ANES Cumulative Data File (1948-2020)

# Citation:
# American National Election Studies. 2022. ANES Time Series
# Cumulative Data File [dataset and documentation]. September 16,
# 2022 version. www.electionstudies.org

set.seed(12345)

# load packages
library(tidyverse)
# library(miguk) # package that houses ANES data sets 'miguk' = America in Korean

# simple function %nin% or %not_in%
'%nin%' <- function(x, table) is.na(match(x, table, nomatch=NA_integer_))

# load data
data("anesCDF", package = 'miguk')

# alternatively
# anesCDF <- rio::import(file = "data-raw/anes_timeseries_cdf_csv_20220916.csv")

# when importing .sav (SPSS) files, `rio::import()` uses `haven::read_sav()`
# anesCDF <- rio::import("~/R/data/ANES/Time Series Cumulative Data File/data/anes_timeseries_cdf_spss_20220916.sav")

# load ANES CDF data dictionary
anesCDF_dict <- readr::read_csv("resources/anesCDF_codebook_dictionary.csv")


#:::::::: Note on the current release of the ANES CDF data set :::::::::::::####

# The current release (2022-09-16) of the Cumulative Data File contains 1030
# variables. Sixty-nine variables have been updated to include data from the
# 2020 Time Series study. Three (VCF9056, VCF9057, and VCF9060) have been
# revised to include the full range (0-100) of feeling thermometer values with
# missing data being recoded to values greater than 900. This release also
# includes a full sample post-election weight variable (VCF9999), for study years
# and cases for which a post-election weight is available.



# VCF0609 NOCARE :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# 1952-1988,1992:
# ‘I don’t think public officials care much what people like me think.’
# 1990,1994-LATER:
# ‘Public officials don’t care much what people like me think.’
# NOSAY 
# 0 = NA
# 1 = Agree, (includes agree strongly and agree somewhat)
# 2 = Disagree, (includes strongly disagree and disagree somewhat from 1998 and later)
# 3 = Neither agree nor disagree (1988 and later only), 
# 9 = Don't know; not sure; it depends; can't say; refused to say

# 2008 NOTE:
# This question was administered to a random half sample of respondents
# (version ‘C’); the remaining respondents were administered an
# alternative version of the question (version ‘D’).
# 2012 NOTE:
# This question was asked of a random 1/2 sample of respondents (the remaining
# 1/2 sample was asked a different version of the question).

# check years where NOCARE was asked
anesCDF |> 
  count_wide(
    rows = VCF0004, # year
    cols = VCF0609  # NOCARE
             ) |> 
  print(n = Inf)


# VCF0613 NOSAY ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# ‘People like me don’t have any say about what the government does.’
# 0 = NA
# 1 = agree, (includes agree strongly and agree somewhat)
# 2 = disagree, (includes strongly disagree and disagree somewhat from 1998 and later)
# 3 = neither, 
# 9 = dk

# Code 1 includes: ‘strongly agree’ and ‘agree’ from the 1966 data
# ‘agree strongly’ and ‘agree somewhat’ from 1988 and later data (exc.
# 2002). Code 2 includes: ‘strongly disagree’ and ‘disagree’ from
# the 1966 data; ‘disagree somewhat’ and ‘disagree strongly’ from 1988
# and later data (exc. 2002).


# 1996 NOTE:
# This question was asked in both the Pre and Post; Pre data are included
# here.
### NOTE: This note from ANES CDF codebook seems important. Only the pre-election data is included in the CDF, so I'd need to grab the 1996 data to determine whether there were any differences. I doubt it, however. Though it is important to note the significance of this question only being asked pre-election, while 'trust-in-gov' and the like are mostly asked only in the post-election period. It is notable because of the seemingly close relationship between external efficacy and trust in gov, not to mention gov responsiveness.

# check years where NOSAY was asked
anesCDF |> 
  # filter(year >=1988) |>
  count_wide(
    rows = VCF0004, # year 
    cols = VCF0613  # NOSAY
    ) |> 
  print(n = Inf)

# VCF0648 External Political Efficacy Index ::::::::::::::::::::::::::::::::####
# 
# Built from VCF0609 and VCF0613
# VCF0613: 'People like me don't have any say about what the government does.'
# VCF0609: 'I don't think public officials care much what people like me think.'
# Year Span 	1952 - 2020
# Data Source 	VCF0648
# Notes: Component variables are first recoded as follows: VCF0609, VCF0613:
# 1=0, 2=100, 3=50. These recoded values are then totaled (Don't Know is not
# scored) and the sum is divided by the number of valid responses. The result is
# then rounded to the nearest integer.
# 100 = most efficacious
# 0 = least efficacious
# 999 = not scored in both VCF0609 and VCF0613

anesCDF |>
  select(VCF0004, VCF0648) |>
  # dplyr::mutate(across(c(VCF0648), ~dplyr::na_if(., 999))) |> 
  count_wide(
    rows = VCF0004, # year
    cols = VCF0648  # External efficacy index 
  ) |> 
  print(n = Inf)

table(anesCDF$VCF0648)


# show years when either NOSAY or NOCARE was not asked :::::::::::::::::::::####

# In the CDF, for the NOCARE and NOSAY variables, 
# code `0` = NA; no Pre IW; no Post IW; split versions: not asked (2008); form B (1986)
# for external efficacy index, code `999` = Not scored in both VCF0609 and VCF0613
# So here, I first re-code those values to explicit NA values.
# Then, show the years where either NOCARE (VCF0609) or NOSAY (VCF0613) were not
# asked

# years where NOCARE was not asked:
# 1948, 1954, 1958, 1962 
anesCDF |> 
  select(VCF0004, VCF0609, VCF0613) |>
  dplyr::mutate(across(c(VCF0609, VCF0613), ~dplyr::na_if(., 0))) |>
  count_wide(
    rows = VCF0004, # year
    cols = VCF0609  # NOCARE 
  ) |> 
  print(n = Inf)

# years where NOSAY was not asked
# 1948, 1954, 1958, 1962, 1986
anesCDF |> 
  select(VCF0004, VCF0609, VCF0613) |>
  dplyr::mutate(across(c(VCF0609, VCF0613), ~dplyr::na_if(., 0))) |>
  count_wide(
    rows = VCF0004, # year
    cols = VCF0613  # NOSAY 
  ) |> 
  print(n = Inf)

# NOTE: the NOSAY external efficacy item was not asked in 1986 for some unknown reason. Neither NOSAY or NOCARE were asked in years 1948, 1954, 1958, 1962, and 2012, but only NOSAY was omitted from the survey in 1986.

# subset data frame ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####



# cdf = cumulative data file
# subset dataframe by desired variables from ANES variable codebook
cdf <- anesCDF |>
  janitor::clean_names() |>
  dplyr::select(
    anes_version          = version,
    year                  = vcf0004,
    case_id               = vcf0006,
    panelcase_id          = vcf0006a,
    weights9x             = vcf0009x,
    weights9y             = vcf0009y,
    weights9z             = vcf0009z,
    weights10x            = vcf0010x,
    weights10y            = vcf0010y,
    weights10z            = vcf0010z,
    weights11x            = vcf0011x,
    weights11y            = vcf0011y,
    weights11z            = vcf0011z,
    weights_vcf9999       = vcf9999,
    post_electionIW       = vcf0013,
    pre_electionIW        = vcf0014,
    cross_sec_comp        = vcf0016,
    interview_mode        = vcf0017,
    age                   = vcf0101,
    age_group             = vcf0102,
    cohort                = vcf0103,
    gender                = vcf0104,
    race_7cat             = vcf0105a,
    race_4cat             = vcf0105b,
    race_3cat             = vcf0106,
    hisp_orgin_type       = vcf0107,
    hisp_orgin            = vcf0108,
    educ_4cat             = vcf0110,
    census_region         = vcf0112,
    political_south       = vcf0113,
    income_group          = vcf0114,
    hh_union_mbr          = vcf0127,
    union_who             = vcf0127b,
    relig_major           = vcf0128,
    relig_7cat            = vcf0128a,
    relig_8cat            = vcf0128b,
    educ_6cat             = vcf0140,
    educ_7cat             = vcf0140a,
    birthplace            = vcf0142,
    parents_native        = vcf0143,
    partyid               = vcf0301,
    partyid_3cat          = vcf0303,
    interest_in_elections = vcf0310,
    care_who_wins         = vcf0311,
    care_who_wins_house   = vcf0312,
    attention             = vcf0313,
    pres_app_anes         = vcf0450,
    pres_app_strngth      = vcf0451,
    protest_approval      = vcf0601,
    civildisobedience     = vcf0602,
    demonstrations        = vcf0603,
    trust1                = vcf0604,
    trust2                = vcf0605,
    trust3                = vcf0606,
    trust4                = vcf0608,
    nocare                = vcf0609,
    nosay                 = vcf0613,
    complex               = vcf0614,
    govrespons_elections  = vcf0624,
    exteff_index          = vcf0648,
    gov_resp_index        = vcf0649,
    trust_gov_index       = vcf0656,
    turnout               = vcf0702,
    regis_turnout         = vcf0703,
    votechoice_candiates  = vcf0704,
    votechoice_2party     = vcf0704a,
    votechoice            = vcf0705,
    voted                 = vcf0706,
    vote_party_house      = vcf0736,
    betteroff             = vcf0880,
    betteroffa            = vcf0880a,
    betteroff_next_year   = vcf0881,
    congress_district     = vcf0900,
    pres_app_econ         = vcf9009
  )

# Recode `999` values and filter data frame ::::::::::::::::::::::::::::::::####

# recode `999` as NA for external efficacy, gov responsiveness, and trust in gov
# indices
cdf <- cdf |> 
  mutate(across(c(exteff_index, gov_resp_index, trust_gov_index), ~dplyr::na_if(., 999)))

# for many variables/items, `0` = NA; no Post IW, etc. So recode `0` as NA
# it's too risky to simply blanket re-code every `0` as NA for every variable,
# so I'll only do the few I know for sure are coded in that way.
cdf <- cdf |>
  dplyr::mutate(across(c(nosay, nocare), ~dplyr::na_if(., 0)))

# check out difference in NA values now
table(anesCDF$VCF0648, useNA = "always")
table(cdf$exteff_index, useNA = "always")

# omit the years where external efficacy items were not asked
cdf <- cdf |> 
  dplyr::filter(year %nin% c(1948, 1954, 1958, 1962))


# create subset data codebook/dictionary and rename columns ::::::::::::::::####

# save character vector of new column names corresponding to ANES var names 
# (technically a named character object)
new_var_names <- c(
  anes_version          = "version",
  year                  = "vcf0004",
  case_id               = "vcf0006",
  panelcase_id          = "vcf0006a",
  weights9x             = "vcf0009x",
  weights9y             = "vcf0009y",
  weights9z             = "vcf0009z",
  weights10x            = "vcf0010x",
  weights10y            = "vcf0010y",
  weights10z            = "vcf0010z",
  weights11x            = "vcf0011x",
  weights11y            = "vcf0011y",
  weights11z            = "vcf0011z",
  weights_vcf9999       = "vcf9999",
  post_electionIW       = "vcf0013",
  pre_electionIW        = "vcf0014",
  cross_sec_comp        = "vcf0016",
  interview_mode        = "vcf0017",
  age                   = "vcf0101",
  age_group             = "vcf0102",
  cohort                = "vcf0103",
  gender                = "vcf0104",
  race_7cat             = "vcf0105a",
  race_4cat             = "vcf0105b",
  race_3cat             = "vcf0106",
  hisp_orgin_type       = "vcf0107",
  hisp_orgin            = "vcf0108",
  educ_4cat             = "vcf0110",
  census_region         = "vcf0112",
  political_south       = "vcf0113",
  income_group          = "vcf0114",
  hh_union_mbr          = "vcf0127",
  union_who             = "vcf0127b",
  relig_major           = "vcf0128",
  relig_7cat            = "vcf0128a",
  relig_8cat            = "vcf0128b",
  educ_6cat             = "vcf0140",
  educ_7cat             = "vcf0140a",
  birthplace            = "vcf0142",
  parents_native        = "vcf0143",
  partyid               = "vcf0301",
  partyid_3cat          = "vcf0303",
  interest_in_elections = "vcf0310",
  care_who_wins         = "vcf0311",
  care_who_wins_house   = "vcf0312",
  attention             = "vcf0313",
  pres_app_anes         = "vcf0450",
  pres_app_strngth      = "vcf0451",
  protest_approval      = "vcf0601",
  civildisobedience     = "vcf0602",
  demonstrations        = "vcf0603",
  trust1                = "vcf0604",
  trust2                = "vcf0605",
  trust3                = "vcf0606",
  trust4                = "vcf0608",
  nocare                = "vcf0609",
  nosay                 = "vcf0613",
  complex               = "vcf0614",
  govrespons_elections  = "vcf0624",
  exteff_index          = "vcf0648",
  gov_resp_index        = "vcf0649",
  trust_gov_index       = "vcf0656",
  turnout               = "vcf0702",
  regis_turnout         = "vcf0703",
  votechoice_candiates  = "vcf0704",
  votechoice_2party     = "vcf0704a",
  votechoice            = "vcf0705",
  voted                 = "vcf0706",
  vote_party_house      = "vcf0736",
  betteroff             = "vcf0880",
  betteroffa            = "vcf0880a",
  betteroff_next_year   = "vcf0881",
  congress_district     = "vcf0900",
  pres_app_econ         = "vcf9009"
)

# In data dictionary of full ANES CDF, prefix all strings in `label_var` with
# string identifying ANES variable code
anesCDF_dict <- anesCDF_dict |> 
  dplyr::mutate(label_var = str_glue("[{var}]: {label_var}"))

# create a data dictionary of subset dataframe. This retains all of the
# variable, value labels, and value (codes) as the origin ANES CDF data set.
# while also replacing the `var` character strings with the renamed variable
# columns of the subsetted data frame
cdf_dict <- anesCDF_dict |>
  mutate(var = janitor::make_clean_names(var)) |> 
  dplyr::filter(var %in% new_var_names) |> 
  dplyr::mutate(var = names(cdf))

# remove redundant dataframe from environment
rm(anesCDF, anesCDF_dict)

# Save subset of ANES CDF and data dictionary/codebook as rds ::::::::::::::####

readr::write_rds(cdf, file = "data/anes_cdf_subset.rds")
readr::write_rds(cdf_dict, file = "data/anes_cdf_subset_data_dictionary.rds")
