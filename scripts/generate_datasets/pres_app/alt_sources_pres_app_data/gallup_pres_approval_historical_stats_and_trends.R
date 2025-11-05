# Here I am compiling together data I downloaded from Gallup Org historical presidential approval ratings trend site page. 
# [here](https://news.gallup.com/poll/116677/presidential-approval-ratings-gallup-historical-statistics-trends.aspx)

# set seed for reproducibility
set.seed(12345)

# load package
library(tidyverse)

# Import and process Approval ratings data Gathered from Gallup Page :::::::####

# These .csv files were downloaded from the Gallup Org historical presidential
# approval ratings trend site page. [here](https://news.gallup.com/poll/116677/presidential-approval-ratings-gallup-historical-statistics-trends.aspx)

# Note that the Gallup Historical presidential approval ratings page does not
# include ratings for Roosevelt.

gallup_files <- list.files(path = "~/R/data/Gallup/", full.names = T)

# All except the Trump and Obama data set has four columns. That fourth column is redundant and can be excluded. I have to load those with only 4 columns, remove that column for each, and then attach the Trump and Obama data to it.

# Does not contain Trump or Obama
gallup_pres <- readr::read_csv(gallup_files[c(1:10, 12:13)], id = "file") |> 
  janitor::clean_names() |>
  dplyr::select(-file, -x_1)

gallup_pres2 <- readr::read_csv(gallup_files[c(11,14)], id = "file") |> 
  janitor::clean_names() |>
  dplyr::select(-file)

# hold off on binding them together for now.
# gallup_pres <- rbind(gallup_pres, gallup_pres2)

gallup_pres |> dplyr::distinct(president)

gallup_pres <- gallup_pres |>  
  dplyr::mutate(date = dplyr::case_when(
    date == "2021 Jan 21-Feb 2" ~ "2021 Jan 21-Feb 2",
    date == "56 May 31Jun 5" ~ "56 May 31-Jun 5",
    .default = as.character(date)
  ))

gallup_pres <- gallup_pres |> 
  tidyr::separate_wider_delim(
    cols = date, 
    delim = "-", 
    names_sep = "_")

gallup_pres <- gallup_pres |> 
  tidyr::separate_wider_delim(cols = date_1, delim = " ",
                              names = c("year", "start_month", "start_day"))


gallup_pres <- gallup_pres |>
  dplyr::mutate(
    year = dplyr::case_when(
      president %in% c(
        "Eisenhower",
        "Truman",
        "Johnson",
        "Kennedy",
        "Nixon",
        "Ford",
        "Carter",
        "Reagan",
        "Bush"
      ) ~ str_c("19", year),
      president %in% c("BushJr") ~ str_c("20", year),
      
      president == "Clinton" 
      & year %in% c("93", "94", "95", "96", "97", "98", "99")
      ~ str_c("19", year),
      
      president == "Clinton" & year == "00"
      | president == "Clinton" & year == "01" ~ str_c("20", year),
      .default = as.character(year)
    )
  )

gallup_pres <- gallup_pres |>
  separate_wider_regex(cols = date_2,
                       patterns = c(end_month = "\\D*", end_day = "\\d+")) |>
  mutate(end_month = dplyr::case_when(
    end_month == "" ~  coalesce(start_month, end_month),
    .default = as.character(end_month)
  ))

gallup_pres <- gallup_pres |> 
  mutate(
    start = lubridate::ymd(stringr::str_glue("{year}-{start_month}-{start_day}")),
    end = lubridate::ymd(stringr::str_glue("{year}-{end_month}-{end_day}"))
  ) |> 
  dplyr::relocate(approve, .after = end) |> 
  dplyr::arrange(desc(start)) |> 
  dplyr::select(-start_month, -start_day, -end_month, -end_day)

gallup_pres

# now for the Obama and Trump date frame
# NOTE: For Obama, Gallup did weekly averages. Why? idk, but it is just another annoying obstacle.

gallup_pres2

gallup_pres2 <- gallup_pres2 |> 
  tidyr::separate_wider_delim(
    cols = date, 
    delim = " ", 
    names = c("month", "day", "year")) |> 
  dplyr::mutate(day = str_remove(day, ",")) |> 
  dplyr::relocate(year, .before = month) |> 
  mutate(
    start = lubridate::ymd(stringr::str_glue("{year}-{month}-{day}"))
  ) |> 
  dplyr::relocate(approve, .after = start) |> 
  dplyr::arrange(desc(start)) |> 
  dplyr::select(-month, -day)




# Now combine Obama and Trump approval ratings with rest of group
# Note that the surveys/polls for Obama and Trump do not have associated end
# dates
gallup_pres <- dplyr::bind_rows(gallup_pres, gallup_pres2) |>
  dplyr::arrange(desc(start))

# add columns for year and month
gallup_pres <- gallup_pres |>
  dplyr::mutate(
    year = lubridate::year(start),
    month = lubridate::month(start, label = T, abbr = T),
                .before = start)

# there's a mistake in the end dates for two cases
gallup_pres |> filter(end == "1965-01-05" | end == "1954-01-05")

# fix the mistake
gallup_pres <- gallup_pres |> 
  mutate(end = dplyr::case_when(
    end == "1965-01-05" ~ lubridate::ymd("1966-01-05"),
    end == "1954-01-05" ~ lubridate::ymd("1955-01-05"),
    .default = lubridate::ymd(end)
  ))

rm(gallup_pres2)

# save data gleaned from Gallup page as .rds
readr::write_rds(gallup_pres, file = "data/gallup_pres_app_historical_stats_trends.rds")
