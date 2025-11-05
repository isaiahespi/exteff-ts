# Here, all I am doing is compiling the presidential approval ratings data
# derived from The American Presidency Project (APP) at UCSB. NOTE: the data set
# for Donald Trump only contains his first term from 2017 to 2021.


# All the Presidential job approval ratings data are said to have been derived
# and curated from Gallup polls. Although no specific identification code or
# Gallup Poll number is provided.

# set seed for reproducibility
set.seed(12345)

# load package
library(tidyverse)


# list of multiple data sets assigned to character vector
american_pres_proj_files <- list.files(
  path = "~/R/data/American Presidency Project UCSB/",
  pattern = "\\D+\\.csv$", # regex pattern
  full.names = T
)


american_pres_proj <- readr::read_csv(american_pres_proj_files, id = "file") |> 
  janitor::clean_names() |> 
  dplyr::rename(
    start = start_date,
    end = end_date,
    approve = approving, 
    disapprove = disapproving, 
    unsure = unsure_no_data) |> 
  dplyr::mutate(across(c(start, end), ~lubridate::mdy(.))) |> 
  dplyr::arrange(start)

american_pres_proj <- american_pres_proj |>
  dplyr::mutate(
    president = dplyr::case_when(
      file == american_pres_proj_files[4] ~ "Roosevelt",
      file == american_pres_proj_files[8] ~ "Truman",
      file == american_pres_proj_files[3] ~ "Eisenhower",
      file == american_pres_proj_files[10] ~ "Kennedy",
      file == american_pres_proj_files[12] ~ "Johnson",
      file == american_pres_proj_files[13] ~ "Nixon",
      file == american_pres_proj_files[7] ~ "Ford",
      file == american_pres_proj_files[9] ~ "Carter",
      file == american_pres_proj_files[14] ~ "Reagan",
      file == american_pres_proj_files[5] ~ "Bush",
      file == american_pres_proj_files[15] ~ "Clinton",
      file == american_pres_proj_files[6] ~ "BushJr",
      file == american_pres_proj_files[1] ~ "Obama",
      file == american_pres_proj_files[2] ~ "Trump",
      file == american_pres_proj_files[11] ~ "Biden",
    ),
    .after = file
  ) |>
  dplyr::select(-file)

american_pres_proj <- american_pres_proj |> 
  mutate(year = lubridate::year(start),
         month = lubridate::month(start,label = T, abbr = T), 
         .before = start)
  

american_pres_proj

# Note that there are two rows in the APP data that appear to be duplicated. When only selecting three rows, a single duplicate pair of rows is found. However, they differ in approval ratings. These two rows imply one of two things: either Gallup conducted two separate polls on the exact same days on a separate sample in March of 1942 and obtained highly similar results, or one of these rows is in erroneous. I say its the latter. I am unable to verify the APP UCSB data due to the fact that any particular Gallup poll identifiers are absent.
american_pres_proj |> 
  select(president, start, end) |> janitor::get_dupes()

american_pres_proj |> 
  select(president, start, end, approve) |> janitor::get_dupes()


# check to determine whether the Roosevelt approval ratings data sum to 100
# filter to display only those instances where the ratings do not equal 100
american_pres_proj |> 
  group_by(president, start, end) |> 
  mutate(total = sum(approve, disapprove, unsure)) |> 
  filter(total != 100)




# save as .rds file
readr::write_rds(
  american_pres_proj, 
  file = "data/american_pres_proj_UCSantaBarbara_presidential_app_data.rds")
