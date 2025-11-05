# This script scrapes a couple of tables from DOJ website and combines them into
# a single tibble dataframe.

# The result is a dataframe that contains the jurisdictions that were previously subject to preclearance under Section 5 of the Voting Rights Act. The columns of the dataframe include the state, county if the whole state was not subject, the date the preclearance requirement was applicable to the jurisdiction, the Federal Register citation  (archive/identification code), and the date recorded in the Federal Register.

# state_covered
# county_covered
# date_applicable: Official start date for when preclearance requirement applied
# fed_register: Federal Register Citation
# date_FR: Federal Register publication date

# set seed for reproducibility
set.seed(12345)

# load packages (those commented out may or may not be necessary)
library(tidyverse)
library(rvest) # Easily Harvest (scrape) web pages

# Identify jurisdictions previously subject to preclearance 

# this reads all tables on the page and returns them in a list
# you can save those tables, and then use magrittr to extract specific ones
doj_tabs <-
  rvest::read_html(
    "https://www.justice.gov/crt/jurisdictions-previously-covered-section-5") %>%
  rvest::html_table(fill = TRUE, header = TRUE) 
# fill = TRUE fills in missing values with NA
# header = TRUE indicates that the first row contains the header names.

# this will output the first table in the console as a tibble
# if you want a different table further down the web page,
# then you have to specify the nth table
preclear_states <- doj_tabs %>%
  magrittr::extract2(1) |> 
  dplyr::rename(
    "state_covered" = 'States Covered as a Whole',
    "date_applicable" = 'Applicable Date',
    'fr_citation' = 'Fed. Register',
    'date_fr' = 'Date'
  ) |> 
  # convert dates to proper date class
  dplyr::mutate(date_applicable = lubridate::mdy(date_applicable),
                date_fr = lubridate::mdy(date_fr))


preclear_counties <- doj_tabs %>%
  magrittr::extract2(2) |>
  dplyr::rename(
    "state_covered" = 1,
    'counties_covered' = 2,
    'date_applicable' = 'APPLICABLE DATE',
    'fr_citation' = 'Fed. Register',
    'date_fr' = 'Date'
  ) |> 
  # replace blank "" with explicit NA
  dplyr::mutate(across(everything(), ~dplyr::case_when(
    .x == "" ~ NA, .default = as.character(.x)
  ))) |>
  # remove colon from strings in every row of column
  dplyr::mutate(state_covered = stringr::str_remove(state_covered, ":")) |>
  # fill in empty rows with prior character string ('last obs carried forward')
  dplyr::mutate(state_covered = zoo::na.locf(object = state_covered)) |>
  tidyr::drop_na() |>  
  # fix minor typo in one of the dates
  dplyr::mutate(
    date_fr = stringr::str_replace(
      date_fr, "Sept. 23.", "Sept. 23,")) |> 
  # convert dates to proper date class
  dplyr::mutate(date_applicable = lubridate::mdy(date_applicable),
                date_fr = lubridate::mdy(date_fr))

# NOTE: There were two "Townships" in Two different counties in Michigan, but
# those are not relevant; County-level data on ANES respondents is only
# available if using restricted data with permission. Also, respondents of a particular county would not be representative of said county since ANES method is for sample to be nationally representative. 

# combine the two tables together
preclearance <- dplyr::bind_rows(preclear_states, preclear_counties) |> 
  dplyr::relocate(counties_covered, .after = state_covered) |> 
  dplyr::mutate(counties_covered = tidyr::replace_na(counties_covered, "All"))

# NOTE: In South Dakota, Shannon County was renamed to Oglaga Lakota County in 2015.
preclearance <- preclearance |> 
  dplyr::mutate(counties_covered = dplyr::case_when(
    counties_covered == "Shannon County" ~ "Oglala Lakota County",
    .default = as.character(counties_covered)
  ))


preclearance <- preclearance |>  
  dplyr::rename(st = state_covered, cnty = counties_covered) |> 
  dplyr::mutate(st_fips = fipio::as_fips(st), .after = st)

preclearance <- preclearance |> 
  dplyr::filter(cnty == "All") |> 
  dplyr::bind_rows(
    preclearance |>
      dplyr::filter(cnty != "All") |>
      dplyr::mutate(
        cntyfips = usmap::fips(state = dplyr::first(st), county = cnty),
        .by = st)
    ) |> 
  dplyr::relocate(cntyfips, .after = cnty)

# save as rds to check out later
readr::write_rds(preclearance, file = "data/jurisdictions_previously_covered.rds")

rm(preclear_states, preclear_counties, doj_tabs)
