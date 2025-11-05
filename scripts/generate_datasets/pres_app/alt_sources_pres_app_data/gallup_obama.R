# get approval ratings for Obama in 2016

# The Roper Center's iPoll Data base does not contain Gallup Org approval ratings data for Obama for the year 2016. This is odd because there Gallup polling trend data is presented on the following URL,
# (https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx)

# This script scrapes the Obama Gallup approval trend data from the URL above, extracts one table in particular, and formats it into a tibble dataframe. 

# The plan is to merge this into the other presidential approval ratings data I have.

# load these 
library(rvest)
library(tidyverse)

# this reads all tables on the page and returns them in a list
# you can save those tables, and then use magrittr to extract specific ones

gallup_obama_tables <-
  rvest::read_html(
    "https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx") %>%
  rvest::html_table(fill = TRUE, header = TRUE) 
# fill = TRUE fills in missing values with NA
# header = TRUE indicates that the first row contains the header names.

# save as rds to check out later
# I'm saving it in alternate directory on my local computer
readr::write_rds(gallup_obama_tables, file = "~/R/data/Gallup/Obama_Gallup_tables.rds")

# this will output the first table in the console as a tibble
# if you want a different table further down the web page,
# then you have to specify the nth table
obama2016 <- gallup_obama_tables %>%
  magrittr::extract2(2) |> 
  dplyr::select(date = 1, 
                approve = Approve, 
                disapprove = Disapprove, 
                no_opinion = 'No opinion') |> 
  dplyr::filter(!date == "" & !date == "Gallup") |>
  dplyr::mutate(across(c(approve, disapprove, no_opinion), ~as.numeric(.)))

obama2016 <- obama2016 |> 
  tidyr::separate_wider_delim(
    cols = date, 
    delim = "-", 
    names_sep = "_")

obama2016 <- obama2016 |>
  tidyr::separate_wider_delim(
    cols = date_1,
    delim = " ",
    names = c("year", "start_month", "start_day")
  )


obama2016 <- obama2016 |>
  mutate(start = lubridate::ymd(stringr::str_glue("{year}-{start_month}-{start_day}")),
         .before = date_2)

obama2016 <- obama2016 |>  
  dplyr::mutate(date_2 = dplyr::case_when(
    date_2 == "2017 Jan 1" ~ "Jan 1",
    .default = as.character(date_2)
  ))


obama2016 <- obama2016 |> 
  tidyr::separate_wider_regex(
    cols = date_2,
    patterns = c(end_month = "\\D*", end_day = "\\d+"))

obama2016 <- obama2016 |>
  mutate(
    end_month = dplyr::case_when(
      end_month == "" ~  coalesce(start_month, end_month),
      .default = as.character(end_month)
  ))

obama2016 <- obama2016 |> 
  mutate(
    end = lubridate::ymd(stringr::str_glue("{year}-{end_month}-{end_day}")), 
    .after = start
  )

obama2016 <- obama2016 |> 
  dplyr::mutate(end = dplyr::case_when(
    end == "2016-01-01"~ lubridate::ymd("2017-01-01"),
    .default = end
  ))

obama2016 <- obama2016 |> 
  dplyr::arrange(desc(start)) |> 
  dplyr::select(-year, -start_month, -start_day, -end_month, -end_day)

obama2016 <- obama2016 |> 
  dplyr::mutate(president = rep_len(c("Obama"), length.out = n())) |> 
  dplyr::relocate(president, .before = start)

# 2015 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

obama2015 <- gallup_obama_tables %>%
  magrittr::extract2(3) |> 
  dplyr::select(date = 1, 
                approve = Approve, 
                disapprove = Disapprove, 
                no_opinion = 'No opinion') |> 
  dplyr::filter(!date == "" & !date == "Gallup") |>
  dplyr::mutate(across(c(approve, disapprove, no_opinion), ~as.numeric(.)))

obama2015 |> print(n = Inf)


obama2015 <- obama2015 |> 
  tidyr::separate_wider_delim(
    cols = date, 
    delim = "-", 
    names_sep = "_")

obama2015 <- obama2015 |>
  tidyr::separate_wider_delim(
    cols = date_1,
    delim = " ",
    names = c("year", "start_month", "start_day")
  )


obama2015 <- obama2015 |>
  mutate(start = lubridate::ymd(stringr::str_glue("{year}-{start_month}-{start_day}")),
         .before = date_2)

obama2015 <- obama2015 |>  
  dplyr::mutate(date_2 = dplyr::case_when(
    date_2 == "2016 Jan 3" ~ "Jan 3",
    .default = as.character(date_2)
  ))


obama2015 <- obama2015 |> 
  tidyr::separate_wider_regex(
    cols = date_2,
    patterns = c(end_month = "\\D*", end_day = "\\d+"))

obama2015 <- obama2015 |>
  mutate(
    end_month = dplyr::case_when(
      end_month == "" ~  coalesce(start_month, end_month),
      .default = as.character(end_month)
  ))

obama2015 <- obama2015 |> 
  mutate(
    end = lubridate::ymd(stringr::str_glue("{year}-{end_month}-{end_day}")), 
    .after = start
  )

obama2015 <- obama2015 |> 
  dplyr::mutate(end = dplyr::case_when(
    end == "2015-01-03"~ lubridate::ymd("2016-01-03"),
    .default = end
  ))

obama2015 <- obama2015 |> 
  dplyr::arrange(desc(start)) |> 
  dplyr::select(-year, -start_month, -start_day, -end_month, -end_day)

obama2015 <- obama2015 |> 
  dplyr::mutate(president = rep_len(c("Obama"), length.out = n())) |> 
  dplyr::relocate(president, .before = start)


# combine 2016 and 2015 tables
obama_tabs <- obama2016 |> dplyr::bind_rows(obama2015)

obama_tabs <- obama_tabs |> dplyr::rename(unsure = no_opinion)

obama_tabs <- obama_tabs |> dplyr::mutate(
  survey_org = rep_len(c("Gallup Organization"), length.out = n()),
  source_doc = rep_len(c("Gallup Poll"), length.out = n()),
  question_txt = rep_len(c("Do you approve or disapprove of the way Barack Obama is handling his job as president?"), length.out = n()),
  citation = rep_len(c("https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx"), length.out = n()))

obama_tabs <- obama_tabs |> 
  dplyr::mutate(
    year = lubridate::year(start), 
    month = lubridate::month(start, label = T, abbr = T),
    quarter = lubridate::quarter(start, type = "quarter"),
    .before = approve
  )


# clear memory
rm(obama2015, obama2016, gallup_obama_tables)
