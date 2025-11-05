# This script is used to compose the data set that will be used in the
# replication and extended analysis.


# NOTE on data sets imported and used in this script ----------------------

# Since the replication data for Chamberlain (2012) was not readily available, the dataset generated here is composed of three different data sets each gathered independently and generated from resources available to me at the time of writing. Each data set contains one or more of the relevant variables necessary to replicate Chamberlain's analysis. 

# The external efficacy and trust in government variables were obtained from the ANES Cumulative data file (ANES CDF). Appropriate weights were applied to the ANES CDF data set and subset to included a selection of variables. The ANES CDF is imported into the R environment and assigned as `cdf_wt`. The unweighted ANES CDF data set is imported and assigned as `cdf`.

# Presidential approval ratings data from the Gallup Organization derived from the Roper Center's iPoll database. Note that the Roper Center's iPoll Data base did not contain Gallup Org approval ratings data for Obama for the year 2016. I supplemented that gap in the Presidential approval ratings data by splicing in approval ratings data for Obama covering years 2015 and 2016 scraped from the Gallup Polling Trend data site (https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx). The resulting data set is imported in and assigned as `roper`.

# The next data set contains the index of consumer sentiment variable, which was readily available and easy to obtain from the University of Michigan (https://www.sca.isr.umich.edu/tables.html). [see also (https://data.sca.isr.umich.edu/data-archive/mine.php#)].

# The gathering and generation process for each of these data sets is documented in their respective script files in the `generate_datasets` directory.


# -------------------------------------------------------------------------

# 1) replication of data used by Chamberlain (2012) consisting only of the
# dependent and independent variables of Chamberlain's 2012 study
# 2) full dataset that includes time since Chamberlain's 2012 publication

# The replica dataset is intended to replicate the figures and
# results from Chamberlain (2012).


# set up ------------------------------------------------------------------

# set seeed
set.seed(12345)

# load some custom functions
source(here::here('utils', 'funs.R'))

# load packages
# library(tidyverse)
# library(srvyr)

# load ANES Time Series Cumualative Data File (CDF)
# cdf <- readr::read_rds(file = "data/anes data/anes_cdf.rds")

# load cdf as tbl_svy object (i.e., with weight variable applied)
cdf_wt <- readr::read_rds(file = "data/anes data/anes_cdf_wts.rds")

# also load 2024 ANES survey data (saved as survey design object)
anes2024 <- readr::read_rds(file = "data/anes2024des.rds")

# load presidential approval ratings data derived from Roper Center iPOLL
roper <- readr::read_rds("data/roper/roper_toplines_pres_approval.rds")

# load ics data
ics <- readr::read_rds("data/ics/index_consumer_sentiment.rds")


# Replication data set ----------------------------------------------------

# This section joins data frames together to create a data set
# that consists of only the dependent and independent variables of Chamberlain's
# study
# In the combined data set, each row should consist of observations pertaining
# to a particular year, and each column refers to one of the variables in the
# model.

# Not every variable has values for each year, and the
# variables of interest are lag-versions with different lags (e.g., external
# efficacy lag of 4 years, whereas yearly average for presidential approval is
# lagged by one year)

# Right now, the ANES CDF contains repeated cross-sectional data at the
# individual-level spanning across multiple decades. However, I only need index
# variables by year; I don't need individual-level response data. 

# Dependent variable: external efficacy index
# Three (or four, technically) predictors: 
# 1) exteff_lag4y: external efficacy index lag 4-years
# 2) presapp_lag1y : yearly average presidential approval ratings, lag 1-year
# 3) ics_lag1y : yearly average of quarterly index of consumer sentiment, lag 1-year
# 4) trustgov_lag2y: average yearly score on the trust in government index, lag 2-years  
# I need to create a data set where each row consists of observations pertaining
# to a particular year, and each column refers to one of the variables in the
# model.

# average aggregate external efficacy and trust in gov by year 
# using the srvyr function, I am able to get standard error estimates as well
# NOTE: Trust in gov index was not measured consistently until 1964
# NOTE: external efficacy items were not asked in years 1948, 1954, 1958, 1962
# NOTE: the "NOSAY" external efficacy item was not asked in 1986; index value
# reflects average of "NOCARE" item.

anes_indices <- cdf_wt |>
  # derive mean with standard errors for each index by year
  srvyr::summarise(
    dplyr::across(
      c(exteff.indx, trustgov.indx),
      ~ srvyr::survey_mean(.x, na.rm = T)),
    .by = year)

# The ANES Index values represent yearly averages, so any value that equals 0.00
# indicates that the particular index was not measured for that year.

# replace any value less than or equal to 0 as NA
anes_indices <- anes_indices |>
  dplyr::mutate(across(
    c(exteff.indx, exteff.indx_se, trustgov.indx, trustgov.indx_se),
    ~ dplyr::na_if(., 0.0000000)
  ))


# Also incorporate the average external efficacy index value derived from the
# 2024 ANES data. I coded the `exteff.cdf` variable following the coding scheme
# of the ANES CDF. Variable coding for the trust-in-government items slightly
# differed for 2024 compared to ANES CDF, but I computed the trust-in-government
# index according to the ANES CDF documentation.

# extract averages of 2024 external efficacy index and trust in government index
# add year column with 2024 as the year
anes2024_indices <- anes2024 |> 
  srvyr::summarise(
  exteff.indx = srvyr::survey_mean(exteff.cdf, na.rm = T),
  trustgov.indx = srvyr::survey_mean(trustgov.indx, na.rm = T)
) |> 
  dplyr::mutate(year = as.integer(2024), .before = exteff.indx)

# merge the anes_indices from the CDF with the 2024 ANES indices
# this simply incorporates the most recent 2024 ANES data 
anes_indices <- dplyr::full_join(anes_indices, anes2024_indices)

# obtain yearly averages of quarterly ICS from 1960 to 2025
econ_sentiment <- ics |> 
  dplyr::reframe(ics = mean(ics.q),
          .by = year) |>
  dplyr::filter(year >= 1960) |> 
  dplyr::arrange(year)

# NOTE: there's incomplete quarterly data for the index of consumer sentiment
# before 1960. There is ICS data prior to 1960 as far back as 1952 in the
# monthly ICS data. However, the monthly data prior to 1960 contains ICS values
# for three months per year at best (1952 only has data for November). The ICS
# survey didn't consistently ask all component questions of the ICS until 1960,
# and the survey wasn't conducted monthly until 1978.

# average approval over time
approval <- roper |>
  dplyr::reframe(presapp = mean(approve),
          .by = year) |> 
  dplyr::arrange(year)

# merge index variables, approval ratings, and ICS into dataframe (wide format) 
d <- dplyr::full_join(approval, econ_sentiment, by = dplyr::join_by(year)) |> 
  dplyr::full_join(anes_indices, by = dplyr::join_by(year))

# NOTE: putting data frames in a list and merging via
# purrr::reduce(dplyr::full_join) also works, but will drop years where no
# exteff.indx was measured. I aim to retain all years for every variable that
# has data, so I don't use this method, but I show it here for reference.
# list(
#   anes_indices = anes_indices,
#   approval = approval,
#   econ_sentiment = econ_sentiment
# ) |> 
#   purrr::reduce(dplyr::full_join, by = "year")

# include modified ICS values following Chamberlain (2012)
d <- d |> 
  # include modified ICS values following Chamberlain
  dplyr::mutate(ics_modified = (ics-50), .after = ics)

# year can't be in `<dbl>` numeric class, must be integer disallowing decimals
d$year <- as.integer(d$year)

# drop standard error estimates columns; not needed
d <- d |>
  dplyr::select(-dplyr::contains("_se"))

# remove `.indx` suffix for external efficacy and trust in gov indices
d <- d |> 
  dplyr::rename_with(.cols = c(exteff.indx, trustgov.indx),
                     .fn = \(x) stringr::str_remove(x, pattern = ".indx"))


# pivot data to long format -----------------------------------------------

# convert dataframe to long form
dlong <- d |> 
  tidyr::pivot_longer(cols = -year, names_to = "variable", values_to = "value")

# create lags -------------------------------------------------------------

# create lagged versions of dependent variable and independent predictor
# variables.

# In a data set where each row corresponds to a particular year,
# `dplyr::lag(x, n = 1, ordered_by = year)` will shift x such that lagged values
# reflect the values from the prior year. If the values of x are missing (NA)
# for the year prior, then a 1-year lag of x will be NA for that particular
# year.

# create lagged versions of variables ordered by year
dlong <- dlong |>
  dplyr::arrange(variable, year) |>
  dplyr::group_by(variable) |>
  timetk::tk_augment_lags(.value = -year, .lags = 1:4) |> 
  dplyr::rename_with(
    .cols = c(-year, -variable, -value), 
    .fn = \(x) stringr::str_replace(x, pattern = "value_lag", replacement = "T")
    )

# alternative equivalent
# dlong |> 
#   dplyr::arrange(variable, year) |> 
#   dplyr::group_by(variable) |> 
#   dplyr::mutate(T1 = dplyr::lag(value, n = 1L, order_by = year),
#                 T2 = dplyr::lag(value, n = 2L, order_by = year),
#                 T3 = dplyr::lag(value, n = 3L, order_by = year),
#                 T4 = dplyr::lag(value, n = 4L, order_by = year)
#                 )


# pivot back to wide format, arrange by year in descending order
d <- dlong |>
  tidyr::pivot_wider(
    id_cols = year, 
    names_from = variable, 
    names_glue = "{variable}.{.value}",
    values_from = c(value, T1, T2, T3, T4)) |> 
  dplyr::arrange(dplyr::desc(year))

# remove string `.value` from column names
d <- d |>  
  dplyr::rename_with(
    .cols = -year, 
    .fn = \(x) stringr::str_remove(x, pattern = ".value"))

# check
# d |>
#   dplyr::filter(year >= 1952 & year <= 2024) |>
#   dplyr::select(year, dplyr::contains("exteff"))

# create numeric interval corresponding to general election years, i.e., every 4
# years since 1940
election_yrs <- seq(1940, max(d$year), by = 4)

# create numeric interval of years corresponding to midterm election years,
# i.e., every 2 years since 1938
midterm_yrs <- election_yrs-2

# create factor categorical variable identifying election years since 1952, 
# general, midterm
d <- d |>
  dplyr::mutate(election_yr = forcats::fct(
    dplyr::case_when(
      year %in% election_yrs ~ "General",
      year %in% midterm_yrs ~ "Midterm",
      TRUE ~ NA
    ),
    levels = c("General", "Midterm")
  ),
  .after = year)


# create lags of prior values, i.e., by observation ---------------------------

# In a data set where each row corresponds to a particular year, a lag of x t-1
# will shift x such that lagged values reflect the values from the time minus
# one. In this case, the units of time are years, to t-1 = the current year
# minus one. If the values of x are missing (NA) for the year prior, then a
# 1-year lag of x will be NA for that particular year.

# Taking the same dataset where each row consecutively corresponds to years in sequence, there may be values of column `x` that are missing for a particular year. If the years (rows) with missing values of `x` are dropped, then a lag of `x` by 1 will reflect the value of `x` at its prior observation. For instance, a lag of `x` by 2 will reflect its 2nd previously observed value, and so on.

# create another dataframe where the variables are lagged by observation
dlong2 <- dlong |> 
  dplyr::select(variable, year, value) |> 
  # dropping missing data (NA) from long format is key here
  tidyr::drop_na() |> 
  timetk::tk_augment_lags(.value = -year, .lags = 1:4) |> 
  dplyr::rename_with(
    .cols = c(-year, -variable, -value), 
    .fn = \(x) stringr::str_replace(x, pattern = "value_lag", replacement = "L")
    )

# pivot wider to wide form dataframe with values lagged by observation
d2 <- dlong2 |> 
  tidyr::pivot_wider(
    id_cols = year, 
    names_from = variable, 
    names_glue = "{variable}.{.value}",
    values_from = c(value, L1, L2, L3, L4)) |> 
  dplyr::arrange(dplyr::desc(year)) |>  
  dplyr::rename_with(
    .cols = -year, 
    .fn = \(x) stringr::str_remove(x, pattern = ".value"))


# left join long form dataset
dlong <- dlong |> dplyr::left_join(dlong2)

d <- d |> dplyr::left_join(d2)

# remove temporary data frames from environment
rm(d2, dlong2)

# save data frames as .rds files :::::::::::::::::::::::::::::::::::::::::::####

# save d in wide format
readr::write_rds(d, file = "data/replica data/replica_data_wide.rds")

# save d in long format
readr::write_rds(dlong, file = "data/replica data/replica_data_long.rds")

# Remove objects from memory :::::::::::::::::::::::::::::::::::::::::::::::####

# remove all objects from envir/memory
rm(list = ls())

