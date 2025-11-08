# This script replicates Figure 1 from Chamberlain (2012)

# Chamberlain, Adam. 2012. “A Time-Series Analysis of External Efficacy.” Public Opinion Quarterly 76(1): 117–30. doi:10.1093/poq/nfr064.


# set up ------------------------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load some custom functions
source(here::here('utils', 'funs.R'))

# required packages
# library(tidyverse)
# library(naniar) # data structures, summaries, and visuals for missing data
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data

# load cdf as tbl_svy object (i.e., with weight variable applied)
cdf_wt <- readr::read_rds(file = "data/anes data/anes_cdf_wts.rds")


# prepare data for plotting -----------------------------------------------

# This figure requires the cumulative data file as a `tbl_svy` object (`cdf_wt`)

# convert `nocare` and `nosay` to factor vars
cdf_wt <- cdf_wt |>
  srvyr::mutate(across(c(nocare, nosay), ~ sjlabelled::set_labels(
    .x, labels = c(
      "Agree" = 1,
      "Disagree" = 2,
      "Neither" = 3,
      "DK" = 9
    )
  ))) |>
  srvyr::mutate(across(c(nocare, nosay), ~ sjlabelled::as_label(., prefix = TRUE)))


# the custom `svy_count_group()` function returns a table of response
# proportions for a single variable (factor)
# using this table, I am able to get the proportion of "Agree", "Disagree" etc.
# for the `NOCARE` and `NOSAY` variables by year.

# create two tables for NOSAY and NOCARE variables
nocare_by_yr <- svy_count_group(dat = cdf_wt, var = nocare, group_var = year)
nosay_by_yr <- svy_count_group(dat = cdf_wt, var = nosay, group_var = year)

# class(nocare_by_yr) # note class is no longer tbl_svy object

# bind rows of tables (basically stack one on top of the other)
resp_tab_by_year <- nosay_by_yr |> 
  dplyr::bind_rows(nocare_by_yr) |> 
  dplyr::arrange(year)

# `resp_tab_by_year` is in long format. To replicate Figure 1, I filter to
# "Disagree" responses and plot lines representing percent who "Disagree" to
# each question across time

# NOTE: since `NOSAY` was not asked in 1986, the function draws a line for NOSAY
# between 1984 and 1988 implicitly via linear interpolation. It is easier to
# visualize where `NOSAY` was not asked when by including layer of point values
# via `geom_point`


# Replicate Figure 1 from Chamberlain (2012) ------------------------------

# Figure 1: Percent of Respondents Who Feel (Externally) Efficacious for Each Question

# Figure 1. Percent of Respondents Who Feel Efficacious for Each Question NOTE:
# This doesn't expand the y-axis to range from 0-100, which mirrors Figure 1 in
# Chamberlain (2012). Also limit the data to what was available to Chamberlain
# at the time

fig1.replica <- resp_tab_by_year |>
  dplyr::filter(Response == "[2] Disagree", year <= 2008) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, linetype = Variable))+
  ggplot2::geom_line()+
  ggplot2::scale_linetype_manual(
    values = c("NOSAY" = "solid", "NOCARE" = "dashed"), 
    labels = c("NOSAY" = "No Say", "NOCARE" = "Don't Care"))+
  ggplot2::scale_x_continuous(
    limits = c(1950, 2010),
    breaks = seq(1950, 2010, by = 10),
    expan = ggplot2::expansion(0.01,0.01))+
  ggplot2::scale_y_continuous(n.breaks = 6)+
  ggplot2::labs(
    title = "Percent of Respondents Who Feel Efficacious for Each Question",
    subtitle = "Replication of Figure 1 in Chamberlain (2012)",
    caption = stringr::str_wrap("Source data: ANES Time Series Cumulative Data File. Note that the `NOSAY` question was not included in the 1986 ANES"),
    x = "Year",
    y = "Percent")+
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )
  
# Now same plot, but this time the y-axis is scaled to range from 0-100 and
# point values for each response proportion value is added to each line for
# better comparison. A single point highlights the year `NOSAY` was not asked.
# Data from subsequent ANES surveys since 2008 is also included

fig1.adj <- resp_tab_by_year |> 
  # temporarily ungroup dataframe; can't add row to grouped dataframe
  dplyr::ungroup() |> 
  # add single row of missing data (omitted columns are NA by default)
  tibble::add_row(tibble::tibble_row(
    year = 1986, Variable = "NOSAY", Response = "[2] Disagree"), .after = 87) |>
  # re-group by year
  dplyr::group_by(year) |>
  # include only "Disagree" responses
  dplyr::filter(Response == "[2] Disagree") |>
  # create plot
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, linetype = Variable))+
  ggplot2::geom_line()+
  # add point values including points for missing values. 
  # prop_below: place 75% above min point value
  naniar::geom_miss_point(prop_below = -0.75)+
  # ggplot2::geom_point()+
  ggplot2::scale_linetype_manual(
    values = c("NOSAY" = "solid", "NOCARE" = "dashed"), 
    labels = c("NOSAY" = "No Say", "NOCARE" = "Don't Care"))+
  ggplot2::scale_colour_manual(
    values = c("Missing" = "red"),
    labels = c("Missing" = "Not Asked", "Not Missing" = "Asked")
  )+
  ggplot2::scale_x_continuous(
    limits = c(1950, 2020),
    breaks = seq(1952, 2020, by = 4),
    expan = ggplot2::expansion(0.01,0.01))+
  ggplot2::scale_y_continuous(
    limits = c(0,100),
    # expansion gets rid of padding
    expand = ggplot2::expansion(0,0))+
  ggplot2::labs(
    title = "Percent of Respondents Who Disagree with External Efficacy Item Statements",
    subtitle = "Adjusted Replication of Figure 1 in Chamberlain (2012)",
    caption = stringr::str_wrap("Source data: ANES Time Series Cumulative Data File"),
    x = "Year",
    y = "Percent")+
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )

