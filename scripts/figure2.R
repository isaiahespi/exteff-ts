# This script replicates Figure 2 from Chamberlain (2012)

# Chamberlain, Adam. 2012. “A Time-Series Analysis of External Efficacy.” Public Opinion Quarterly 76(1): 117–30. doi:10.1093/poq/nfr064.


# Set up ------------------------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# required packages
# library(tidyverse)
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data
# library(survey) # Analysis of Complex survey samples

# load data in long format
dlong <- readr::read_rds(file = "data/replica data/replica_data_long.rds")



# Replicate data ----------------------------------------------------------

# the `replica` data set is constrained to the data (presumably) available to
# Chamberlain at the time
replica_long <- dlong |> dplyr::filter(year >= 1952 & year <= 2008)


# Replicate Figure 2 from Chamberlain (2012) ------------------------------

# Figure 2 has plot lines of the three chosen predictors over time. 
# Note that in a footnote (note 7), Chamberlain states, 
# "The Index of Consumer Sentiment was rescaled for figure 2 by subtracting 50 from all its values (to approximate a measure that would fit on a JOO-point scale)"

# exact replication of Figure 2, modifying ICS values in way described by
# Chamberlain (2012)
fig2.replica <- replica_long |>
  # temporarily remove lagged variables from data frame
  dplyr::select(-dplyr::contains("T")) |>
  # drop rows that contain missing values (NA)
  tidyr::drop_na() |>  
  # exclude average values for external efficacy index and ics (not modified)
  dplyr::filter(!variable %in% c("exteff", "ics")) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, linetype = variable)) +
  ggplot2::geom_line() +
  ggplot2::scale_linetype_manual(
    values = c(
      "ics_modified" = "solid",
      "presapp" = "dashed",
      "trustgov" = "twodash"
    ),
    labels = c(
      "ics_modified" = "Index of Consumer Sentiment",
      "presapp" = "Presidential Approval",
      "trustgov" = "Trust in Government"
    )
  ) +
  ggplot2::scale_x_continuous(
    limits = c(1950, 2010),
    breaks = seq(1950, 2020, by = 10),
    expan = ggplot2::expansion(0.01, 0.01)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 100, by = 20),
    # expansion gets rid of padding
    expand = ggplot2::expansion(0.05, 0.05)) +
  ggplot2::labs(
    title = "Changes to Index of Consumer Sentiment, Presidential Approval, and Trust in Government",
    subtitle = "Replication of Figure 2 from Chamberlain (2012)",
    caption = stringr::str_wrap(
      "Source data: ANES Time Series Cumulative Data File; The Gallup Organization; Survey of Consumers, University of Michigan.", width = 95),
    x = "Year",
    y = "Scale"
  ) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )


# Extended Replica of Figure 2 --------------------------------------------


# fig-cap for following plot: "Replication of Figure 2 found in @chamberlain2012 extended to include all years for which data was available. Reflects average Values of the Index of Consumer Sentiment, Presidential Approval, and Trust in Government Over Time. ICS values have not been re-scaled and the Y-Axis is expanded to preserve full range of values. Source data: ANES Time Series Cumulative Data File; The Gallup Organization; Survey of Consumers, University of Michigan."

# extended replica of Figure 2. Don't re-scale values; don't cut years.
fig2.adj <- dlong |>
  # temporarily remove lagged variables from data frame
  dplyr::select(-dplyr::contains("T")) |>
  # drop rows that contain missing values (NA)
  tidyr::drop_na() |>
  # exclude average values for external efficacy index and ics (modified)
  dplyr::filter(!variable %in% c("exteff", "ics_modified")) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, linetype = variable)) +
  ggplot2::geom_line() +
  # ggplot2::geom_point(mapping = aes(colour = variable))+
  ggplot2::scale_linetype_manual(
    values = c(
      "ics" = "solid",
      "presapp" = "dashed",
      "trustgov" = "twodash"
    ),
    labels = c(
      "ics" = "ICS",
      "presapp" = "Presidential Approval",
      "trustgov" = "Trust in Government"
    )
  ) +
  ggplot2::scale_x_continuous(
    limits = c(1938, 2025),
    breaks = seq(1930, 2025, by = 5),
    expan = ggplot2::expansion(0.01, 0.01)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 110, by = 10),
    # expansion controls padding at limit y-axis limits
    expand = ggplot2::expansion(0.01, 0.01)) +
  ggplot2::labs(
    caption = stringr::str_wrap("Source data: ANES Time Series Cumulative Data File; The Gallup Organization; Survey of Consumers, University of Michigan.", width = 95),
    x = "Year",
    y = "Average"
  ) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )



# Compare Exact and Adjusted Replicas of Figure 2 -------------------------

# I present a Figure that includes both the unadjusted and adjusted versions
# of yearly average ICS values over time.

# same replica chart of Figure 2, except now includes non-modified version of
# ICS yearly average values
fig2.compare <- replica_long |>
  # temporarily remove lagged variables from data frame
  dplyr::select(-dplyr::contains("T")) |>
  # drop rows that contain missing values (NA)
  tidyr::drop_na() |> 
  # exclude average values for external efficacy index
  dplyr::filter(!variable %in% c("exteff")) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, linetype = variable, colour = variable)) +
  ggplot2::geom_line() +
  ggplot2::scale_linetype_manual(
    values = c(
      "ics" = "solid",
      "ics_modified" = "solid",
      "presapp" = "dashed",
      "trustgov" = "twodash"
    ),
    labels = c(
      "ics" = "Index of Consumer Sentiment",
      "ics_modified" = "Index of Consumer Sentiment (Modified)",
      "presapp" = "Presidential Approval", 
      "trustgov" = "Trust in Government"
    )
  ) +
  ggplot2::scale_color_manual(
    values = c("ics" = "black", "ics_modified" = "red",
               "presapp" = "azure4", "trustgov" = "darkgray"),
    labels = c("ics" = "Index of Consumer Sentiment",
               "ics_modified" = "Index of Consumer Sentiment (Modified)",
               "presapp" = "Presidential Approval", 
               "trustgov" = "Trust in Government")
  )+
  ggplot2::scale_x_continuous(
    limits = c(1950, 2010),
    breaks = seq(1950, 2020, by = 10),
    expan = ggplot2::expansion(0.01, 0.01)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, by = 20),
    # expansion gets rid of padding
    expand = ggplot2::expansion(0.05, 0.05)) +
  ggplot2::labs(
    caption = stringr::str_wrap("Source data: ANES Time Series Cumulative Data File; The Gallup Organization; Survey of Consumers, University of Michigan.", width = 95
    ),
    x = "Year",
    y = "Average"
  ) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# In a note, Chamberlain states, "The Index of Consumer Sentiment was rescaled for figure 2 by subtracting 50 from all its values (to approximate a measure that would fit on a JOO-point scale)." [@chamberlain2012, 120, n7].

# The data available to Chamberlain at the time ranged from 1952 to 2008 for Presidential approval ratings (Gallup); 1960 to 2008 for quarterly ICS data (UMich), and of course, 1958 and 1964 to 2008 for trust in government (ANES).     

# Just an aside: I think his decision to rescale the ICS by subtracting 50 from the mean value for each year presents a chart that is visibly misleading. On visual inspection of Figure 2 in the original publication, the average values for aggregate consumer sentiment across time appear far lower than the actual average values derived from the un-adjusted data. The variable isn't transformed but a particular value is subtracted from the data; transforming the values of the data keeps the data intact. This was done for the sake of fitting the data to range on a scale between 0 to 100. This seems fine, but the method of modification seems arbitrary. Although this Figure alone was not the point of the study, the visualization of the data as such easily presents a misleading comparison of the values across time. I would imagine that modifying the data for the sake of this figure alone would not have passed peer review, but evidently, it did. 

# Alternate transformations for Figure 2 :::::::::::::::::::::::::::::::::::####

# extended replica of Figure 2, normalize values to range from 0 to 100
# Note that this also presents a misleading visual of the data over time.
fig2.normal <- dlong |>
  # temporarily remove lagged variables from data frame
  dplyr::select(-dplyr::contains("T")) |>
  # drop rows that contain missing values (NA)
  tidyr::drop_na() |>
  # exclude average values for external efficacy index and ics (modified)
  dplyr::filter(!variable %in% c("exteff", "ics_modified")) |>
  dplyr::mutate(value_normal = ((value - min(value, na.rm = T)) / (max(value, na.rm = T) -
                                                                     min(value, na.rm = T))) * 100) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = value_normal, linetype = variable)) +
  ggplot2::geom_line() +
  # ggplot2::geom_point(mapping = aes(colour = variable))+
  ggplot2::scale_linetype_manual(
    values = c(
      "ics" = "solid",
      "presapp" = "dashed",
      "trustgov" = "twodash"
    ),
    labels = c(
      "ics" = "ICS",
      "presapp" = "Presidential Approval",
      "trustgov" = "Trust in Government"
    )
  ) +
  ggplot2::scale_x_continuous(
    limits = c(1938, 2025),
    breaks = seq(1930, 2025, by = 5),
    expan = ggplot2::expansion(0.01, 0.01)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    # expansion controls padding at limit y-axis limits
    expand = ggplot2::expansion(0.01, 0.01)
  ) +
  ggplot2::labs(
    caption = stringr::str_wrap(
      "Source data: ANES Time Series Cumulative Data File; The Gallup Organization; Survey of Consumers, University of Michigan.",
      width = 95
    ),
    x = "Year",
    y = "Averages"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )


# extended replica of Figure 2, standard values to z-scores (center at mean = 0
# with standard deviation = 1)
fig2.z <- dlong |>
  # temporarily remove lagged variables from data frame
  dplyr::select(-dplyr::contains("T")) |>
  # drop rows that contain missing values (NA)
  # tidyr::drop_na() |>
  # exclude average values for external efficacy index and ics (modified)
  dplyr::filter(!variable %in% c("exteff", "ics_modified")) |>
  # adjust values to mirror data available to Chamberlain at the time 
  dplyr::mutate(value = dplyr::case_when(
    variable %in% c("presapp") & year <= 1952 ~ NA,
    .default = value
  )) |>
  tidyr::drop_na(value) |> 
  dplyr::group_by(variable) |> 
  dplyr::mutate(
    value_z = scale(value) |> as.vector()) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value_z, linetype = variable, colour = variable)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 0, color = "darkred")+
  # ggplot2::geom_point(mapping = aes(colour = variable))+
  ggplot2::scale_linetype_manual(
    values = c(
      "ics" = "solid",
      "presapp" = "dashed",
      "trustgov" = "twodash"
    ),
    labels = c(
      "ics" = "ICS",
      "presapp" = "Presidential Approval",
      "trustgov" = "Trust in Government"
    )
  ) +
  ggplot2::scale_discrete_manual(
    aesthetics = c("colour"),
    values = c(
      "ics" = "red",
      "presapp" = "seagreen",
      "trustgov" = "royalblue"))+
  ggplot2::scale_x_continuous(
    limits = c(1950, 2025),
    breaks = seq(1950, 2025, by = 5),
    expan = ggplot2::expansion(0.01, 0.01)
  ) +
  ggplot2::scale_y_continuous(
    # expansion controls padding at limit y-axis limits
    expand = ggplot2::expansion(0.01, 0.01)
    ) +
  ggplot2::labs(
    caption = stringr::str_wrap("Variables have been centered to mean = 0 and a standard deviation = 1. Source data: ANES Time Series Cumulative Data File; The Gallup Organization; Survey of Consumers, University of Michigan.", width = 95),
    x = "Year",
    y = "Z"
  ) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )


