# this script is my addition to the replication. 
# Chamberlain's dynamic time series model with lagged dependent variable is insufficient to properly test the validity and congruence of both meaning and measurement of external efficacy. The theoretical justification for the method is lacking, and the chosen predictors are not tenable as factors representative of the political environment. 

# Rather, I evaluate external efficacy over time from before and after 2013 when the Supreme Court invalidated the preclearance formula of the Voting Rights Act. After that ruling, many states since have passed a significant number of laws pertaining to voter ID, voter registration, methods of voting, and much more. Some states have passed laws that have are incredibly restrictive, yet other states have also passed laws aiming to expand voting rights. Of particular interest are states that passed restrictive laws immediately following the Shelby v Holder ruling. 

# What I do here is conduct a simple difference-in-difference (DID), two-way fixed effects linear regression model. All that should be needed for that is the dependent variable, unit variable, and time.


# set up ------------------------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load packages (those commented out may or may not be necessary)
library(tidyverse)
# library(rvest) # Easily Harvest (scrape) web pages
# library(naniar) # data structures, summaries, and visuals for missing data
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data
# library(cspp) # Correlates of State Policy Project data
# library(modelsummary)
# library(tinytable)

# load some custom functions
source(here::here('utils', 'funs.R'))

# load ANES Time Series Cumulative Data File (CDF)
cdf <- readr::read_rds(file = "data/anes data/anes_cdf.rds")

# load cdf as tbl_svy object (i.e., with weight variable applied)
cdf_wt <- readr::read_rds(file = "data/anes data/anes_cdf_wts.rds")

# load codebook
cdf_codebook <- readr::read_rds(file = "data/anes_cdf_subset_data_codebook.rds")

# load in subset of Correlates of State Policy Project Data (cspp_df)
cspp_df <- readr::read_rds("data/cspp_subset.rds")
cspp_dict <- readr::read_rds("data/cspp_subset_data_dictionary.rds")

# load in dataframe of previously covered jurisdictions
# slightly modify column names and add state FIPS codes
# covered <- readr::read_rds("data/jurisdictions_previously_covered.rds") |>  
#   dplyr::rename(covered_sts = st, covered_cnty = cnty)

# load NCSL enacted state legislation database 2011-2024
stlaws <- readr::read_rds("data/ncsl_enacted_state_legislation_2011_2024.rds")
ncsl_dict <- readr::read_rds("data/ncsl_archived_db_2011_2024_dict.rds")

# use `readxl` package to import covi values from 1996-2024
covi <- readxl::read_xlsx(path = "data-raw/covi/COVI Values 1996-2024 website.xlsx") |> 
  dplyr::mutate(year = as.integer(year)) |> 
  dplyr::relocate(year, .before = statenu)

covi <- covi |> 
  dplyr::rename(st_abb = state) |>
  dplyr::mutate(st_fips = fipio::as_fips(st_abb), .after = st_abb)

covi <- covi |> 
  dplyr::mutate(st = fipio::fips_state(st_fips), .before = st_abb)



# create tibbles in long and wide format for easier plotting --------------

# average external efficacy index by year and region, long format
erlong <- cdf_wt |>
  srvyr::filter(!is.na(exteff)) |>
  srvyr::group_by(year.f, census_region) |> 
  srvyr::cascade(exteff = srvyr::survey_mean(exteff, na.rm = T),
                 .fill = "National")

# average external efficacy index by year and region, wide format
erwide <- erlong |> 
  dplyr::select(-exteff_se) |> 
  tidyr::pivot_wider(names_from = census_region,  values_from = exteff) |> 
  dplyr::rename_with(~stringr::str_remove(., pattern = "^\\[[1-4]+\\]\\s+")) |> 
  dplyr::rename("North_Central" = 'North Central') |> 
  dplyr::mutate(across(where(is.numeric), ~round(., 1)))


# plot of external efficacy by region -------------------------------------

# Average values of external efficacy index by year and region 
plot_extff_by_region <- erlong |> 
  dplyr::mutate(
    census_region = stringr::str_remove(
      census_region, 
      pattern = "^\\[[1-4]+\\]\\s+")) |>
  srvyr::filter(census_region != "National") |> 
  ggplot2::ggplot(ggplot2::aes(x = year.f, y = exteff, 
                      group = census_region, 
                      colour = census_region, 
                      linetype = census_region, shape = census_region))+
  ggplot2::geom_line()+
  ggplot2::geom_point(size = 2)+
  ggplot2::scale_linetype_manual(
    values = c(
      "North Central" = "dashed",
      "Northeast" = "dotted",
      "South" = "dotdash",
      "West" = "twodash"))+
  # ggplot2::scale_colour_manual(values = c("National" = "red"))+
  ggplot2::scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0, 100, by = 5),
    # expansion gets rid of padding
    expand = ggplot2::expansion(0,0))+
  ggplot2::labs(
    caption = 
      stringr::str_wrap("Source data: ANES Time Series Cumulative Data File"),
    x = "Year",
    y = "Average Score on Index")+
  ggplot2::theme_bw()+
  ggplot2::theme(
    axis.text.x      = ggplot2::element_text(angle = 90),
    axis.title.x     = ggplot2::element_blank(),
    legend.position  = "bottom",
    legend.title     = ggplot2::element_blank(),
    panel.grid       = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank())

# Census Regions
# 0. NA (1948); 
# 1. Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT); 
# 2. North Central (IL, IN, IA, KS, MI, MN, MO, NE, ND);
# 3. South (AL, AR, DE, D.C., FL, GA, KY, LA, MD, MS, NC); 
# 4. West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA)

# cdf unweighted ----------------------------------------------------------


# For the indices in the ANES CDF, code `999` refers to missing/NA
# recode `999` as NA for external efficacy, gov responsiveness, and trust in gov
# indices
cdf <- cdf |> 
  dplyr::mutate(across(c(exteff, govresp, trustgov), 
                       ~dplyr::na_if(., 999)))

# for many variables/items, `0` = NA; no Post IW, etc. So recode `0` as NA
# it's too risky to simply blanket re-code every `0` as NA for every variable,
# so I'll only do the few I know for sure are coded in that way.
cdf <- cdf |>
  srvyr::mutate(across(
    c(nosay, nocare, trust1, trust2, trust3, trust4),
    ~ dplyr::na_if(., 0)
  ))

cdf <- cdf |>
  dplyr::mutate(
    st_abb = dplyr::na_if(st_abb, "99"),
    census_region = dplyr::na_if(census_region, 0),
    stcd_fips = dplyr::na_if(stcd_fips, 9999),
    stcd_abb = dplyr::na_if(stcd_abb, "9999")
  ) |>
  dplyr::mutate(census_region = sjlabelled::set_labels(
    census_region,
    labels = c(
      "Northeast" = 1,
      "North Central" = 2,
      "West" = 3,
      "South" = 4
    )
  )) |>
  dplyr::mutate(political_south = sjlabelled::set_labels(
    political_south, 
    labels = c(
      "South" = 1, "Non-South" = 2
      ))) |>
  dplyr::mutate(across(
    c(census_region, political_south),
    ~ sjlabelled::as_label(., prefix = TRUE)
  )) |>
  dplyr::mutate(dplyr::across(c(st_abb, stcd_abb), ~forcats::as_factor(.)))


# year variable also needs to be a factor
cdf <- cdf |> 
  dplyr::mutate(year.f = forcats::as_factor(year), .after = year)


cdf |> dplyr::glimpse()


# ANES sample sizes per interview mode ------------------------------------

# get unweighted sample size by year
# NOTE: these unweighted sample sizes match those reported in the ANES CDF
# codebook
cdf |>
  dplyr::filter(year %in% c(2012, 2016, 2020)) |>
  dplyr::summarise(n = dplyr::n(), .by = year)

cdf |>
  dplyr::select(interview_mode) |> 
  sjlabelled::get_labels(values = 'p')

# VCF0017: Mode of interview
# 0 = All personal
# 1 = Telephone pre (personal interview post or no post interview)
# 2 = Telephone post (personal interview pre-election)
# 3 = All telephone (pre-post studies [no post-only phone in time series])
# 4 = All internet
# 5 = All video (2020 only)


cdf |>
  # filter to include only years with internet sample and by mode of interview 
  dplyr::filter(year %in% c(2012, 2016, 2020)) |>
  dplyr::summarise(
    n = dplyr::n(),
    .by = c(year, interview_mode))


# including only years where there are COVI values available, get unweighted
# sample size per state for each year.
cdf |>
  # filter to include only years with internet sample and by mode of interview 
  dplyr::filter(year %in% c(2012, 2016, 2020),
                interview_mode %in% c(4, 3, 5)) |>
  # dplyr::group_by(year, st_abb) |>
  dplyr::summarise(n = dplyr::n(), .by = c(year, st_fips)) |>
  tidyr::pivot_wider(
    id_cols = st_fips,
    names_from = year,
    names_prefix = "y",
    values_from = n
  ) |> 
  dplyr::mutate(st_abb = fipio::fips_abbr(st_fips), .before = st_fips) |> 
  dplyr::arrange(st_fips) |> 
  print(n = Inf)

# (internet) Sample size for the three years for each state are pretty low. It
# is likely a better idea to check out sample size by region

cdf |>
  # filter to include only years with internet sample and by mode of interview 
  dplyr::filter(year %in% c(2012, 2016, 2020),
                interview_mode %in% c(4, 3, 5)) |>
  # dplyr::group_by(year, st_abb) |>
  dplyr::summarise(n = dplyr::n(), .by = c(year, census_region)) |>
  tidyr::pivot_wider(
    id_cols = census_region,
    names_from = year,
    names_prefix = "y",
    values_from = n
  )




# DID model ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####



cdf_wt |> 
  srvyr::filter(!is.na(exteff)) |> 
  survey::svyglm(
    design = _, 
    formula = exteff ~ census_region + year) |> 
  summary()
  
cdf_wt |> 
  srvyr::filter(!is.na(exteff)) |> 
  srvyr::group_by(year, census_region) |>
  srvyr::summarise(
    exteff = srvyr::survey_mean(exteff)
  )




# boom --------------------------------------------------------------------


# count item responses by year
# (I wasn't able to figure out a way to iterate this)
cdf_wt |> srvyr::filter(year >= 1952) |> svy_count_group(var = nosay, group_var = year)
cdf_wt |> srvyr::filter(year >= 1952) |> svy_count_group(var = nocare, group_var = year)
cdf_wt |> srvyr::filter(year >= 1952) |> svy_count_group(var = trust1, group_var = year)
cdf_wt |> srvyr::filter(year >= 1952) |> svy_count_group(var = trust2, group_var = year)
cdf_wt |> srvyr::filter(year >= 1952) |> svy_count_group(var = trust3, group_var = year)
cdf_wt |> srvyr::filter(year >= 1952) |> svy_count_group(var = trust4, group_var = year)

cdf_wt |> 
  srvyr::drop_na(nosay) |> 
  srvyr::group_by(year, nosay) |> 
  srvyr::survey_count(nosay)

cdf_wt |> 
  srvyr::drop_na(trust1) |> 
  srvyr::group_by(year, trust1) |> 
  srvyr::survey_count(trust1)

cdf_wt |>
  srvyr::filter(year %in% c(2012, 2016, 2020)) |>
  srvyr::group_by(year, st_abb) |>
  srvyr::summarise(tot = srvyr::survey_total()) |>
  tidyr::pivot_wider(
    id_cols = st_abb,
    names_from = year,
    names_prefix = "y",
    values_from = tot
  )


# regress exteff on covi --------------------------------------------------

dplyr::glimpse(covi)


cdf |> 
  dplyr::select(year, year.f, exteff, st_abb, census_region, case_id, interview_mode) |> 
  dplyr::filter(year == 2020,, interview_mode %in% c(3, 4, 5)) |> 
  tidyr::drop_na(exteff) |> 
  dplyr::left_join(y = covi, by = c("year", "st_abb")) |> 
  lm(exteff ~ FinalCOVI, data = _) |> 
  summary()
  





# multi-level model -------------------------------------------------------

# okay so I think a multi-level model for a particular year is where I'll start, say 2012 for now, using the internet only sample.
# the DV is external efficacy index values at the individual level
# the IV is a state's COVI for the year in question
# the group or multilevel predictor is census region: North Central, Northeast,
# South, West.


# So I believe that COVI would be considered a group-level predictor


lme4::lmer()
covi
cdf

cdf |> 
  dplyr::select(year, year.f, exteff, st_abb, census_region, case_id, interview_mode) |> 
  dplyr::filter(year == 2020,, interview_mode %in% c(3, 4, 5)) |> 
  tidyr::drop_na(exteff) |> 
  lme4::lmer(exteff ~ 1 + (1 | census_region), data = _) |> 
  summary()
  
cdf |> 
  dplyr::select(year, year.f, exteff, st_abb, census_region, case_id, interview_mode) |> 
  dplyr::filter(year == 2020,, interview_mode %in% c(3, 4, 5)) |> 
  tidyr::drop_na(exteff) |> 
  dplyr::left_join(y = covi, by = c("year", "st_abb")) |> 
  lme4::lmer(exteff ~ 1 + (1 + FinalCOVI | st_abb), data = _) |> 
  summary()


cdf |> 
  dplyr::select(year, year.f, exteff, st_abb, census_region, case_id, interview_mode) |> 
  dplyr::filter(year == 2020,, interview_mode %in% c(3, 4, 5)) |> 
  tidyr::drop_na(exteff) |> 
  dplyr::left_join(y = covi, by = c("year", "st_abb")) |> 
  lme4::lmer(exteff ~ 1 + (1 + FinalCOVI | census_region), data = _) |> 
  summary()


# I have no idea how to read results in the console, actually. Goddamnit.
