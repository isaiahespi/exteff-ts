

# set seed for reproducibility
set.seed(12345)

# load package
library(tidyverse)
# library(rio)


# compiling Roper center's trends data sets ::::::::::::::::::::::::::::::::####

roper_trend_files <- list.files("~/R/data/Roper iPoll/roper_trends", 
                                pattern = "Roper-Trend\\D+\\.csv", 
                                full.names = T)

roper_trends_raw <- readr::read_csv(roper_trend_files, id = "file") 

# convert to tibble, clean names; convert date to proper class; rename columns;
# reorganize columns; arrange by start date of survey
roper_trends <- roper_trends_raw |> 
  dplyr::as_tibble() |> 
  janitor::clean_names() |> 
  # convert dates from character class to proper date class
  dplyr::mutate(start_date = lubridate::mdy(start_date),
                file = basename(file)) |> 
  dplyr::rename(question_id = question_ref_id,
                start = start_date,
                question_txt = question_wording,
                survey_sponsor = sponsor,
                survey_org = survey_organization,
                sample_size = sample,
                int_method = collection_mode,
                sub_population = subpopulation,
                sample_desc = universe
                ) |> 
  dplyr::relocate(approve, disapprove, 
                  no_opinion, sample_size,
                  sub_population, sample_desc, 
                  sample_notes, 
                  survey_org, survey_sponsor,  
                  .after = start) |> 
  dplyr::arrange(start)


# add a column for president
roper_trends <- roper_trends |> 
  mutate(president = dplyr::case_when(
    stringr::str_detect(file, "Truman") ~ "Truman",
    stringr::str_detect(file, "Eisenhower") ~ "Eisenhower",
    stringr::str_detect(file, "Kennedy") ~ "Kennedy",
    stringr::str_detect(file, "Johnson") ~ "Johnson",
    stringr::str_detect(file, "Nixon") ~ "Nixon",
    stringr::str_detect(file, "Ford") ~ "Ford",
    stringr::str_detect(file, "Carter") ~ "Carter",
    stringr::str_detect(file, "Bush") ~ "BushJr",
    stringr::str_detect(file, "Clinton") ~ "Clinton",
    stringr::str_detect(file, "Obama") ~ "Obama",
    stringr::str_detect(file, "Trump") ~ "Trump"
  ), 
  .before = start)

# add columns for year and month 
roper_trends <- roper_trends |> 
  mutate(year = lubridate::year(start),
         month = lubridate::month(start, label = T, abbr = T), .before = start)

# place file column to end of data frame
roper_trends <- roper_trends |> 
  dplyr::relocate(file, .after = response_categories)

# NA if "*" ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# there are rows where "no_opinion" has "*" as a value. 
roper_trends |> dplyr::filter(no_opinion == "*")

# these are the question_id codes where "*" was entered
roper_trends |> dplyr::filter(no_opinion == "*") |> pull(question_id) |> dput()
c("USGALLUP.200335.Q01", "USGALLUP.2009TR0811.Q01", "USGALLUP.201217.Q01", 
"31116767", "31118598", "31118722")

# transform all "*" to explicit NA values
roper_trends <- roper_trends |>
  dplyr::mutate(no_opinion = dplyr::na_if(no_opinion, "*")) 

# fill unsure where approve + disapprove != 100 ::::::::::::::::::::::::::::####

# here are all the missing values (NA) in the `no_opinion` column
roper_trends |> dplyr::filter(is.na(no_opinion))

# NA values in `no_opinion` are fine, as this column refers to either a response
# of "No opinion/Don't know", "Refused" or any instances.

# However, most cases where approve and disapprove sum to less than 100 have
# response options as either  "No opinion", "Don't know" "Undecided" or
# something indicating no response data. 

# convert `no_opinion` column to numeric, then multiply ratings columns by 100
# and convert to integers
roper_trends <- roper_trends |>
  # convert from character to numeric (double)
  dplyr::mutate(no_opinion = as.numeric(no_opinion)) |> 
  # transform from proportion to percentage proportion
  dplyr::mutate(across(c(approve, disapprove, no_opinion), ~.x * 100)) |>
  # percentages reflecting proportions can't be floating point numbers
  dplyr::mutate(across(c(approve, disapprove, no_opinion), ~as.integer(.))) |> 
  dplyr::rename(unsure = no_opinion) |> 
  dplyr::mutate(archive_number = as.character(archive_number))

# this works but only 1,335 sum to 1, whereas all others are either < 1 or > 1.
# The margin above or below 1 is minuscule, however, so it's not a huge concern.

# output shows that, in 733 cases, approval ratings don't sum to 100 percent 
roper_trends |> 
  dplyr::mutate(
    sum_total = approve+disapprove+unsure,
    .after = unsure
    ) |> 
  select(archive_number, question_id, president, start, approve, disapprove, 
         unsure, sum_total, response_categories) |>
  filter(sum_total > 100 | sum_total < 100) |> print(n = Inf)

# save roper_trends data set as .rds :::::::::::::::::::::::::::::::::::::::####

readr::write_rds(roper_trends, file = "data/roper_trends_pres_app.rds")
