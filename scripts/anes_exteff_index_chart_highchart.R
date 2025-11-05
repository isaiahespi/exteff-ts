# This script replicates the External Political Efficacy Index Chart found on The ANES Guide to Public Opinion and Electoral Behavior [link](https://electionstudies.org/data-tools/anes-guide/anes-guide.html?chart=external_efficacy_index)

# load packages
library(tidyverse)
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data

# load cdf as tbl_svy object (i.e., with weight variable applied)
cdf_wt <- readr::read_rds(file = "data/anes_cdf_wts_tbl_svy.rds")

# plot of yearly average of external efficacy index ::::::::::::::::::::::::####

# using the `highcharts` that seems to be used on ANES site page. 
# From the package site page
# "Highcharter is a R wrapper for Highcharts javascript library and its modules.
# Highcharts is very flexible and customizable javascript charting library and
# it has a great and powerful API"

# it's a little tricky, as there are no default arguments; all arguments need to
# be named
# see here (https://jkunst.com/highcharter/articles/highcharts-api.html)
# also (https://api.highcharts.com/highcharts/)

# Still, it's kinda neat.

anes_exteff_chart <- cdf_wt |>
  # omit years 1948, 1954, 1958, and 1962
  srvyr::filter(!year %in% c(1948, 1954, 1958, 1962)) |>
  srvyr::filter(year >= 1952) |>
  srvyr::summarise(across(
    c(exteff, trustgov),
    ~ srvyr::survey_mean(.x, na.rm = T, vartype = "ci")
  ), .by = year) |>
  highcharter::hchart(
    "line",
    highcharter::hcaes(x = year, y = round(exteff, 1)),
    name = "Average Score on Index",
    color = "#5bbcd6",
    showInLegend = TRUE
  ) |>
  highcharter::hc_xAxis(
    tickInterval = 4,
    tickLength = 0,
    title = list(text = "")
  ) |>
  highcharter::hc_yAxis(
    tickInterval = 10,
    min = 0,
    max = 100,
    title = list(text = "")
  ) |>
  highcharter::hc_title(text = "External Political Efficacy Index") |>
  highcharter::hc_subtitle(text = "Average External Efficacy") |>
  highcharter::hc_caption(
    text = "Replication of chart found on The ANES Guide of Public Opinion and Electoral Behavior",
    hrep = "https://electionstudies.org/data-tools/anes-guide/anes-guide.html?chart=external_efficacy_index",
    enabled = TRUE) |>
  highcharter::hc_credits(
    text = "Chart created using R and package `highcharter`",
    href = "https://jkunst.com/highcharter",
    enabled = TRUE)

# Replicate External Efficacy Index Chart from ANES site, PDF ::::::::::::::####

# The plot generated using the `highcharter` package likely won't render to PDF.
# The following code is a valiant attempt to produce the same plot for PDF

anes_exteff_chart_pdf <- cdf_wt |> 
  # omit years 1948, 1954, 1958, and 1962
  srvyr::filter(!year %in% c(1948, 1954, 1958, 1962)) |>
  srvyr::filter(year >= 1952) |> 
  srvyr::summarise(across(c(exteff, trustgov), 
                          ~ srvyr::survey_mean(.x, na.rm = T, vartype = "ci")), 
                   .by = year) |> 
  ggplot2::ggplot(aes(x = year, y = round(exteff, 1)))+
  ggplot2::geom_line(color = "#5bbcd6", linewidth = 2)+
  # ggplot2::geom_point()+
  ggplot2::scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                              expan = ggplot2::expansion(0.01,0.01))+
  ggplot2::scale_y_continuous(limits = c(0,100),
                              # expansion gets rid of padding
                              expand = ggplot2::expansion(0,0))+
  ggplot2::labs(
    title = "External Political Efficacy Index",
    subtitle = "Average External Efficacy",
    caption = stringr::str_wrap("Replication of chart found on The ANES Guide of Public Opinion and Electoral Behavior"),
    x = element_blank(),
    y = element_blank())+
  ggplot2::theme(
    axis.ticks.x.bottom = element_blank(), 
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

