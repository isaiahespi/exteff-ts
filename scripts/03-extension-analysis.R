# this script examines whether external efficacy 1) differs substantially and signicantly for people subject to different political conditions (i.e., different states or regions), and  2) whether external efficacy shifts when conditions of the political environment change.

# also, external efficacy should be 

# set up ------------------------------------------------------------------

# set seed
set.seed(12345)

# load some custom functions
source(here::here('utils', 'funs.R'))

# packages
# library(tidyverse)
# library(fs)
# library(censusapi)
# library(tidycensus)
# library(survey) # Analysis of Complex survey samples
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data


# load ANES 2024 panel data ANES Panel 2016-2020-2024
panel <- readr::read_rds(file = "data/anes data/anes_2016_2020_2024_panel_survey_des.rds")

# load in ANES 2024 data subset codebook
panel_codebook <- readr::read_rds(
  file = "resources/anes resources/anes_panel_2016_2020_2024_codebook.csv")
