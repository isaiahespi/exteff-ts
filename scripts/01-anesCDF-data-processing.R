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

# Add variable labels, value labels, and create and save data dictionary :::####

# load variable list (this is an .csv file converted by Adobe Acrobat from the
# PDF codebook varlist to an Excel file)
anesCDF_varlist <- rio::import(file = "resources/anes_timeseries_cdf_codebook_varlist.csv") |>
  # fix case of cell text in `variable` column
  dplyr::mutate(variable = dplyr::case_when(
    variable == "VERSION" ~ "Version",
    .default = as.character(variable)
  ))

# add variable labels to `anesCDF` data frame ::::::::::::::::::::::::::::::####

# deframe variables with associated variable labels
anesCDF_varlabels <- anesCDF_varlist |> 
  dplyr::rename(label = variable_label) |> 
  dplyr::select(variable, label) |> 
  tibble::deframe()

# assign the variable lables using the splice operator. Labels are assigned via
# matching against the variable name, so variable order does not matter.
anesCDF <- anesCDF |>
  labelled::set_variable_labels(!!!anesCDF_varlabels)

# clean up global environment a little
rm(anesCDF_varlist, anesCDF_varlabels)

# add value labels to `anesCDF` data frame :::::::::::::::::::::::::::::::::####

# SO the time series cumulative data file in `anesr` package is outdated
# compared to what I pulled more recently in placed into my personal `miguk`
# package.

# However, the `anesr` package has the response options that correspond to each
# value code (e.g., [1] Strongly disagree, 0. No Post-election interview) which
# is super helpful. With those, I can check response options and value codes
# while leaving the variables in the data set as named or labelled numeric
# values. Otherwise, in order to get response options for each, I'll have to
# check the official codebooks.

# What I do here is deframe the value labels (label_val in
# `surveytoolbox::data_dict`) in the same way as before and add them to the
# anesCDF data frame.

# load the Timeseries cumulative dataset file from `anesr` package
data(timeseries_cum, package = "anesr")

# generate dictionary from `labelled` R package
dict_anesr <- timeseries_cum |>
  labelled::generate_dictionary()

# Not all of the variables in the `timeseries_cum` have value_labels, so I
# filter the ones that are filled with the character string 'NULL' 

# Here I deframe and save the actual value labels that are not `NULL`
anesr_dict_value_labels <- dict_anesr |> 
  dplyr::filter(value_labels != "NULL") |> 
  dplyr::as_tibble() |> 
  dplyr::select(variable, value_labels) |> 
  tibble::deframe()

# now I add the `anesr` value labels to my anesCDF data frame
anesCDF <- anesCDF |>
  labelled::set_value_labels(!!!anesr_dict_value_labels) 

# assign data dictionary as tibble dataframe using `surveytoolbox` function
anesCDF_dict <- anesCDF |> surveytoolbox::data_dict()

# Now the `anesCDF_dict` has 4 variable columns
# var: the variable identifer in the data set
# label_var: variable label corresponding to each variable column in the dataset
# label_val: value label that gives labels that correspond to each value
# value: the distinct values that correspond to each variable

# save data dictionary to resources directory ::::::::::::::::::::::::::::::####

readr::write_csv(
  x = anesCDF_dict, 
  file = "resources/anesCDF_codebook_dictionary.csv"
  )


# remove items from global environment
rm(
  anesr_dict_labels,
  anesr_dict_value_labels,
  dict_anesr,
  timeseries_cum
)

# The added benefit of doing all that to create a data dictionary .csv file is
# that now the `anesCDF` dataframe contains variable label and value label
# attributes
str(anesCDF)



# subset data frame ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# cdf = cumulative data file
# subset dataframe by desired variables from ANES variable codebook
cdf <- anesCDF |>
  janitor::clean_names() |>
  dplyr::select(
    version, vcf0004, vcf0006, vcf0006a, vcf0009x, vcf0009y, vcf0009z,
    vcf0010x, vcf0010y, vcf0010z, vcf0011x, vcf0011y, vcf0011z, vcf9999, vcf0013, 
    vcf0014, vcf0016, vcf0017, vcf0101, vcf0102, vcf0103, vcf0104, vcf0105a, 
    vcf0105b, vcf0106, vcf0107, vcf0108, vcf0110, vcf0112, vcf0113, vcf0114,
    vcf0127, vcf0127b, vcf0128, vcf0128a, vcf0128b, vcf0140, vcf0140a, vcf0142,
    vcf0143, vcf0301, vcf0303, vcf0310, vcf0311, vcf0312, vcf0313, vcf0450, 
    vcf0451, vcf0601, vcf0602, vcf0603, vcf0604, vcf0605, vcf0606, vcf0608, 
    vcf0609, vcf0613, vcf0614, vcf0624, vcf0648, vcf0649, vcf0656, vcf0702,
    vcf0703, vcf0704, vcf0704a, vcf0705, vcf0706, vcf0736, vcf0880, vcf0880a,
    vcf0881, vcf0900, vcf9009
  )

# create subset data codebook/dictionary and rename columns ::::::::::::::::####

# create a data dictionary of subset dataframe. This retains all of the
# variable, value labels, and value (codes) as the larger data set.
cdf_dict <- cdf |> surveytoolbox::data_dict()

# save character vector of new column names 
# (technically a named character object)
new_var_names <- c(
  version   = "anes_version",
  vcf0004   = "year",
  vcf0006   = "case_id",
  vcf0006a  = "panelcase_id",
  vcf0009x  = "weights9x",
  vcf0009y  = "weights9y",
  vcf0009z  = "weights9z",
  vcf0010x  = "weights10x",
  vcf0010y  = "weights10y",
  vcf0010z  = "weights10z",
  vcf0011x  = "weights11x",
  vcf0011y  = "weights11y",
  vcf0011z  = "weights11z",
  vcf9999   = "weights_vcf9999",
  vcf0013   = "post_electionIW",
  vcf0014   = "pre_electionIW",
  vcf0016   = "cross_sec_comp",
  vcf0017   = "interview_mode",
  vcf0101   = "age",
  vcf0102   = "age_group",
  vcf0103   = "cohort",
  vcf0104   = "gender",
  vcf0105a  = "race_7cat",
  vcf0105b  = "race_4cat",
  vcf0106   = "race_3cat",
  vcf0107   = "hisp_orgin_type",
  vcf0108   = "hisp_orgin",
  vcf0110   = "educ_4cat",
  vcf0112   = "census_region",
  vcf0113   = "political_south",
  vcf0114   = "income_group",
  vcf0127   = "hh_union_mbr",
  vcf0127b  = "union_who",
  vcf0128   = "relig_major",
  vcf0128a  = "relig_7cat",
  vcf0128b  = "relig_8cat",
  vcf0140   = "educ_6cat",
  vcf0140a  = "educ_7cat",
  vcf0142   = "birthplace",
  vcf0143   = "parents_native",
  vcf0301   = "partyid",
  vcf0303   = "partyid_3cat",
  vcf0310   = "interest_in_elections",
  vcf0311   = "care_who_wins",
  vcf0312   = "care_who_wins_house",
  vcf0313   = "attention",
  vcf0450   = "pres_app_anes",
  vcf0451   = "pres_app_strngth",
  vcf0601   = "protest_approval",
  vcf0602   = "civildisobedience",
  vcf0603   = "demonstrations",
  vcf0604   = "trust1",
  vcf0605   = "trust2",
  vcf0606   = "trust3",
  vcf0608   = "trust4",
  vcf0609   = "nocare",
  vcf0613   = "nosay",
  vcf0614   = "complex",
  vcf0624   = "govrespons_elections",
  vcf0648   = "exteff_index",
  vcf0649   = "gov_resp_index",
  vcf0656   = "trust_gov_index",
  vcf0702   = "turnout",
  vcf0703   = "regis_turnout",
  vcf0704   = "votechoice_candiates",
  vcf0704a  = "votechoice_2party",
  vcf0705   = "votechoice",
  vcf0706   = "voted",
  vcf0736   = "vote_party_house",
  vcf0880   = "betteroff",
  vcf0880a  = "betteroffa",
  vcf0881   = "betteroff_next_year",
  vcf0900   = "congress_district",
  vcf9009   = "pres_app_econ"
)


# add column of custom variable names to codebook/dictionary while retaining
# original ANES variable names
cdf_dict <- cdf_dict |> 
  dplyr::mutate(var_rename = new_var_names, .before = var)

# rename column names
colnames(cdf) <- new_var_names

# this adds prefix to each variable label consisting of the ANES variable code
cdf_dict <- cdf_dict |> dplyr::mutate(label_var = str_glue("[{var}]: {label_var}"))

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

# remove redundant dataframe from environment
rm(anesCDF, anesCDF_dict)

# Save subset of ANES CDF and data dictionary/codebook as rds ::::::::::::::####

readr::write_rds(cdf, file = "data/anes_cdf_subset.rds")
readr::write_rds(cdf_dict, file = "data/anes_cdf_subset_data_dictionary.rds")
