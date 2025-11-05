# obtain data to make the Index of Consumer Sentiment.

# Luckily, this index is easy to obtain from the University of Michigan
# (https://www.sca.isr.umich.edu/tables.html)

# See also,
# [index of consumer sentiment](https://data.sca.isr.umich.edu/data-archive/mine.php#)

# Survey and technical documentation can be found at the following links
# [survey information](https://data.sca.isr.umich.edu/survey-info.php)
# [technical information](https://data.sca.isr.umich.edu/technical-docs.php)

# In the publication, Chamberlain states,
# "...the Index of Consumer Sentiment will be used to assess economic performance. The variable, which is a yearly average of consumer confidence in the economy, is available from the University of Michigan. Higher scores indicate greater consumer confidence." [@chamberlain2012, 120]

# Following Chamberlain, I need to obtain a yearly average of the index taken since 1960. Univerisity of Michigan already provides data on the index for each year since 1960, though it may not be an average.  

# However, in section 2 of the Appendix, Chamberlain states,
# "The Index of Consumer Sentiment is compiled through the Surveys of Consumer Attitudes at the University of Michigan...The measure is an average of the quarterly intervals taken since 1960, when the survey began to consistently ask all the component questions of the index in each quarter, though the questions have been asked monthly since 1978." [@chamberlain2012, 128]

# So, it seems that the yearly average is indeed an average of the index at
# quarterly intervals.

# set seed
set.seed(12345)

# load packages
library(tidyverse)

# load in ICS quarterly data
ics_qrtr <- readr::read_csv(
  "~/R/data/consumer sentiment/index_consumer_sentiment_quarterly_tbqics.csv"
  ) |>
  janitor::clean_names() |>
  dplyr::rename(ics = ics_all)

# alternatively, import .csv directly from url
# https://www.sca.isr.umich.edu/files/tbqics.csv
ics_qrtr <- readr::read_csv("https://www.sca.isr.umich.edu/files/tbqics.csv") |>
  janitor::clean_names() |>
  dplyr::rename(year = yyyy, ics = ics_all)

# remove `.` string from `quarter` column. Include a column for quarter number 
ics_qrtr <- ics_qrtr |>
  mutate(quarter_mon = stringr::str_remove_all(quarter, pattern = "[.]*"), .before = year) |>
  mutate(quarter = rep(1:4, times= 66, length.out = 261), .before = year) |> 
  dplyr::relocate(quarter, .before = year)

# save as .rds
readr::write_rds(ics_qrtr, file = "data/quarterly_index_consumer_sentiment.rds")


# MONTHLY ICS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# load in ICS monthly data
# https://www.sca.isr.umich.edu/files/tbmics.csv
ics_month <- readr::read_csv("~/R/data/consumer sentiment/index_consumer_sentiment_monthly_tbmics.csv") |> 
  janitor::clean_names() |> 
  dplyr::rename(ics = ics_all)

# alternatively, import .csv directly from url
ics_month <- readr::read_csv("https://www.sca.isr.umich.edu/files/tbmics.csv") |>
  janitor::clean_names() |>
  dplyr::rename(year = yyyy, ics = ics_all)

# convert `month` into proper format, create column that indicates quarter by month  
ics_month <- ics_month |>  
  mutate(year_month = paste(year, month, sep = "-") |> lubridate::ym()) |> 
  mutate(month = lubridate::month(year_month, label = T, abbr = T),
         quarter = lubridate::quarter(year_month, type = "quarter"))

# save as .rds
readr::write_rds(ics_month, file = "data/monthly_index_consumer_sentiment.rds")

# YEARLY ICS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# load in ICS yearly data
# [data source](https://www.sca.isr.umich.edu/files/tbyics.csv)
ics_year <- readr::read_csv("~/R/data/consumer sentiment/index_consumer_sentiment_yearly_tbyics.csv") |>
  janitor::clean_names() |> 
  dplyr::rename(ics = ics_all)

# alternatively, import .csv directly from url
ics_year <- readr::read_csv("https://www.sca.isr.umich.edu/files/tbyics.csv") |>
  janitor::clean_names() |>
  dplyr::rename(year = yyyy, ics = ics_all)


ics_year |> print(n = Inf)

ics_year <- ics_year |> 
  mutate(year_month = paste(year, month, sep = "-") |> lubridate::ym()) |> 
  mutate(month = lubridate::month(year_month, label = T, abbr = T),
         quarter = lubridate::quarter(year_month, type = "quarter")) |> 
  dplyr::relocate(quarter, .before = ics)

# save as .rds
readr::write_rds(ics_year, file = "data/yearly_index_consumer_sentiment.rds")


# Combine ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

ics_month
ics_qrtr
ics_year

# combine all data frames into one
ics <- dplyr::left_join(
  ics_month,
  ics_qrtr,
  by = dplyr::join_by(year, quarter),
  suffix = c(".m", ".q")
) |>
  dplyr::relocate(ics.m, .before = ics.q) |>
  dplyr::relocate(year_month, .before = ics.m) |>
  dplyr::left_join(ics_year, by = dplyr::join_by(year, quarter, month, year_month)) |>
  dplyr::rename(ics.y = ics)

# save as .rds
readr::write_rds(ics, file = "data/index_consumer_sentiment.rds")


rm(ics_month, ics_qrtr, ics_year)
