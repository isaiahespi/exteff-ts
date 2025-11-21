# this script applies the survey weights to the ANES CDF

# the ANES CDF supplies weights, which must be used. How I incorporate this into
# the analysis, however, isn't clear to me. I am using repeated cross-sectional
# data, but I know of no function or method for using the weights supplied. It's
# far easier to use a single study, but this is a cumulative data set of
# multiple studies. The best approach I see is to construct a survey design
# object

# NOTE: I'm not entirely sure about how to use the weight variables pertaining to the cumulative data file. Creating a survey design object with the supplied weights, strata, and unit variance (primary sampling unit, PSU) is pretty straightforward for a single cross-section survey. But since this data is a cumulative combination of repeated cross-sections over time, there is no strata nor PSU variable included in the CDF. Each year also has a different target population. All this is to say that I am not totally sure if creating a survey design object for the CDF by applying the appropriate weight variable is the best or most accurate approach.

# Since the data is weighed to the sample in ANES surveys, the weights sum to the sample n. This is mostly the case for each year except for minor differences in 2016, 1992, and from 1970 to 1976. I'm aware of the weirdness concerning the weights for the 1970s, but I'm not entirely sure why the summed weights for 2016 and 1992 don't equal the unweighted sample n. Granted these differences are negligible. The weights may be correcting for specific cases for those years.

# In any case, the unweighted sample N and the summed weights for the combined
# samples for each year are almost identical. Applying weights to the CDF
# shouldn't really be an issue. The difference between using the unweighted CDF
# data set and applying the proper weight variable (using
# `srvyr::as_survey_design()`) is minuscule.

# set up ---------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load custom functions
source(here::here("utils", "funs.R"))

# load packages
# library(tidyverse)
# library(fipio)

# load data
cdf <- readr::read_rds(file = "data/anes data/anes_cdf.rds")


# -------------------------------------------------------------------------

# this matches the ANES CDF documentation (see page 2)
# cdf |> dplyr::summarise(
#   wt9x  = sum(weights9x, na.rm = T),
#   wt9y  = sum(weights9y, na.rm = T),
#   wt9z  = sum(weights9z, na.rm = T),
#   wt10x = sum(weights10x, na.rm = T),
#   wt10y = sum(weights10y, na.rm = T),
#   wt10z = sum(weights10z, na.rm = T),
#   wt11x = sum(weights11x, na.rm = T),
#   wt11y = sum(weights11y, na.rm = T),
#   wt11z = sum(weights11z, na.rm = T),
#   wt9999 = sum(weights_VCF9999, na.rm = T),
#   n = dplyr::n(),
#   .by = year
#   ) |> 
#   print(n = Inf)

# verify the years where the external efficacy index was measured
# cdf |>  
#   dplyr::summarise(
#     exteff.indx.avg = mean(exteff.indx, na.rm = T), 
#     .by = year) |> 
#   print(n = Inf)
# Not measured in 1948, 1954, 1958, and 1962



# re-code external efficacy and trust in gov items following ANES CDF --------


# This note is from the ANES CDF documentation regarding how the external
# efficacy index (VCF0648) is computed from the component survey items
# GENERAL NOTE:
# Built from VCF0609 AND VCF0613.
# Component vars are first recoded as follows: VCF0609,VCF0613:
# 1=0,2=100,3=50. The recoded values are then totaled and divided by
# the number of valid responses. The result is then rounded.


# first, re-code `NOCARE` and `NOSAY` items following ANES CDF documentation
cdf <- cdf |> 
  dplyr::mutate(dplyr::across(c(nocare, nosay), ~labelled::labelled(
    dplyr::case_when(.x == 1 ~ 0, .x == 2 ~ 100, .x == 3 ~ 50, TRUE ~ NA),
    labels = c(
      "Agree" = 0,
      "Neither agree nor disagree" = 50,
      "Disagree" = 100
    ),
    label = paste(
      attr(.x, "label"),
      " values recoded to match ANES CDF coding scheme",
      sep = ","
    )
  ),
  .names = "{col}.cdf"
  ))



# This note is from the ANES CDF documentation regarding how the
# trust-in-government (VCF0656) index is computed from the component survey items
# GENERAL NOTE:
# Built from VCF0604, VCF0605, VCF0606, VCF0608.
# Component vars are first recoded as follows:
# VCF0604: 1=0,2=33,3=67,4=100; VCF0605: 1=0, 2=100;
# VCF0606,VCF0608: 1=0,2=50,3=100.
# The recoded values are then totaled and divided by the number of valid
# responses. The result is then rounded.

# cdf |>
#   dplyr::select(trust1, trust2, trust3, trust4) |> 
#   purrr::map(~sjlabelled::get_labels(.x, values = 'p'))

# re-code trust in gov items following the ANES CDF coding scheme 
cdf <- cdf |>
  dplyr::mutate(
    trust1.cdf = dplyr::case_when(
      trust1 == 1 ~ 0,
      trust1 == 2 ~ 33,
      trust1 == 3 ~ 67,
      trust1 == 4 ~ 100,
      TRUE ~ NA
    ),
    trust2.cdf = dplyr::case_when(trust2 == 1 ~ 0, trust2 == 2 ~ 100, TRUE ~ NA),
    trust3.cdf = dplyr::case_when(trust3 == 1 ~ 0, trust3 == 2 ~ 50, trust3 == 3 ~ 100, TRUE ~ NA),
    trust4.cdf = dplyr::case_when(trust4 == 1 ~ 0, trust4 == 2 ~ 50, trust4 == 3 ~ 100, TRUE ~ NA)
  )


# manually compute external efficacy and trust in gov indices ------------------

# compute external efficacy index according to ANES CDF documentation.
# create unipolar external efficacy index variable matching ANES CDF external
# efficacy variable (VCF0648)
# NOTE: this is different from the aggregate average value of the index. This
# variable reflects an individual's 'score' on the index

# create composite mean individual score for trust in government
# sum of means = sum(mean(x1), mean(x2))/k
# k = number of items, variables, elements in set. 
exteff <- cdf |>  
  dplyr::select(case_id, year, nocare.cdf, nosay.cdf) |> 
  dplyr::mutate(exteff = rowMeans(
    dplyr::pick(nocare.cdf, nosay.cdf, -case_id, -year), na.rm=TRUE))


# compute trust in gov index according to ANES CDF documentation.
# create composite mean individual score for trust in government
# sum of means = sum(mean(x1), mean(x2))/k
# k = number of items, variables, elements in set. 
trust <- cdf |>  
  dplyr::select(case_id, year, trust1.cdf, trust2.cdf, trust3.cdf, trust4.cdf) |>
  dplyr::mutate(trustgov = rowMeans(
    dplyr::pick(trust1.cdf, trust2.cdf, trust3.cdf, trust4.cdf, -case_id, -year), na.rm = T))

# round the result
trust <- trust |> 
  dplyr::mutate(trustgov = round(trustgov, digits = 0))

# join
tmp <- dplyr::left_join(exteff, trust)

# remove from env, clear memory
rm(exteff, trust)
gc()

cdf <- cdf |> dplyr::left_join(tmp)

# remove from env, clear memory
rm(tmp)
gc()


# set value labels
cdf <- cdf |>
  labelled::set_value_labels(
    exteff = attr(cdf$exteff.indx, "labels"),
    trustgov = attr(cdf$trustgov.indx, "labels")
  )

# set variable labels
cdf <- cdf |> 
  labelled::set_variable_labels(
    exteff = paste(attr(cdf$exteff.indx, "label"), ", manually computed"),
    trustgov = paste(attr(cdf$trustgov.indx, "label"), ", manually computed")
  )


# check to ensure that the average external efficacy var matches the external
# efficacy index provided by ANES

# manual coding and computation of indices matches ANES CDF computation
# cdf |>
#   dplyr::summarise(
#     exteff_manual = mean(exteff, na.rm = TRUE),
#     exteff.indx = mean(exteff.indx, na.rm = TRUE),
#     trustgov_manual = mean(trustgov, na.rm = TRUE),
#     trustgov.indx = mean(trustgov.indx, na.rm = TRUE),
#     .by = year
#   ) |>
#   print(n = Inf)


# apply weights -----------------------------------------------------------


# The correct weight variable to use VCF0009z
# Since I am using the combined sample (FTF and web) from 2012, and the External
# efficacy items indicate `Type 0` for 1970 time series, then I must use the
# variable `VCF0009z`
# The weight variable to use is the combined sample (i.e., FTF and Web) with
# 1970 Type 0

# NOTE: The government responsiveness index requires use of a different weight
# variable, weights10z (VCF0010z). This is because the gov responsiveness index
# includes 1970 that was Type 1 (as opposed to type 0).

# apply weights
cdf_wt <- srvyr::as_survey_design(weights = weights9z, .data = cdf)

# process -----------------------------------------------------------------

# for many variables in the CDF, `0` = NA; no Post IW, and 9 = DK. This is not
# the case for all variables in the entire CDF, so I set these values as NA for
# only these select few variables.


cdf_wt <- cdf_wt |>
  srvyr::mutate(dplyr::across(
    c(nosay, nocare, trust1, trust2, trust3, trust4),
    ~ sjlabelled::set_na(., na = c(0, 9))
  ))


# For the indices in the ANES CDF, code `999` refers to missing/NA
# recode `999` as NA for external efficacy, gov responsiveness, and trust in gov
# indices

# cdf_wt |>
#   srvyr::as_tibble() |>
#   dplyr::select(exteff, trustgov, exteff.indx, trustgov.indx, govresp.indx) |>
#   sjlabelled::get_labels(values = 'p')

cdf_wt <- cdf_wt |> 
  srvyr::mutate(srvyr::across(c(exteff.indx, govresp.indx, trustgov.indx), 
                       ~dplyr::na_if(., 999)))


# states, census regions, fips, and processing :::::::::::::::::::::::::::::####

# Census Regions
# 0. NA (1948); 
# 1. Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT); 
# 2. North Central (IL, IN, IA, KS, MI, MN, MO, NE, ND);
# 3. South (AL, AR, DE, D.C., FL, GA, KY, LA, MD, MS, NC); 
# 4. West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA)

# NOTE on Census Region (VCF0112) variable: 1948 apparently didn't include
# states, and for 1954, no state variable was present nor was there any other
# location information available. Neither of these years matter since External
# efficacy was not measured

# replace certain values with NA
# convert select geographic variables to factor

cdf_wt <- cdf_wt |>
  srvyr::mutate(
    st_abb = dplyr::na_if(st_abb, "99"),
    census_region = dplyr::na_if(census_region, 0),
    stcd_fips = dplyr::na_if(stcd_fips, 9999),
    stcd_abb = dplyr::na_if(stcd_abb, "9999")
  ) |>
  srvyr::mutate(census_region = sjlabelled::set_labels(
    census_region,
    labels = c(
      "Northeast" = 1,
      "North Central" = 2,
      "West" = 3,
      "South" = 4
    )
  )) |>
  srvyr::mutate(political_south = sjlabelled::set_labels(
    political_south, 
    labels = c(
      "South" = 1, "Non-South" = 2
      ))) |>
  srvyr::mutate(across(
    c(census_region, political_south),
    ~ sjlabelled::as_label(., prefix = TRUE)
  )) |>
  srvyr::mutate(srvyr::across(c(st_abb, stcd_abb), ~forcats::as_factor(.)))


# year variable also needs to be a factor
cdf_wt <- cdf_wt |> 
  srvyr::mutate(year.f = forcats::as_factor(year), .after = year)


# save the ANES CDF with weights applied as .rds file ::::::::::::::::::::::#### 

# just so I don't have to do it again
readr::write_rds(cdf_wt, file = "data/anes data/anes_cdf_wts.rds")

# clear envir
rm(list = ls())
