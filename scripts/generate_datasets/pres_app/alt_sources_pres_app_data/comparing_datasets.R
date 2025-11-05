# My final attempt to replicate the supposed Gallup presidential approval data derived from different places.

# set seed for reproducibility
set.seed(12345)

# simple function %nin% or %not_in%
'%nin%' <- function(x, table) is.na(match(x, table, nomatch=NA_integer_))

# load packages
library(tidyverse)


# load topline presidential approval ratings data from Roper iPoll
roper_toplines <- readr::read_rds("data/roper_toplines_pres_approval.rds")

# load in Roper Trends data on presidential approval ratings
roper_trends <- readr::read_rds(file = "data/roper_trends_pres_app.rds")

# load in approval ratings data from the American Presidency Project at UC Santa Barbara
gallup_app <- readr::read_rds("data/american_pres_proj_UCSantaBarbara_presidential_app_data.rds")

# load in approval data gleaned from Gallup historical statistics and trends page
gallup_site <- readr::read_rds("data/gallup_pres_app_historical_stats_trends.rds")

# load in the combined data frame
gallup_combined <- readr::read_rds("data/presidential_approval_gallup.rds")

# load in the Gallup approval data from Ghitza replication data
gallup_ghitza <- readr::read_rds("data/gallup_pres_approval_Ghitza_et_al_2023.rds")

# what presidents are included in replication data from Ghitza et al. (2022)?
gallup_ghitza |> dplyr::distinct(president)
# No Trump, no Biden

# what presidents are included in combined data set of American Presidency Proj
# and Gallup trends site?
gallup_combined |> dplyr::distinct(president)
# Looks to be all that can be included

# what Presidents are included in the Roper trends curated data?
# No Roosevelt (okay), Reagan, Bush (senior), nor Biden.
roper_trends |> dplyr::distinct(president)

# Roper topline data has full set, from Roosevelt to Trump
roper_toplines |> dplyr::distinct(president)

# Ghitza has pres approval data from early Roosevelt (Aug 1937) up to end of
# Obama's presidency (Jan 2017).
gallup_ghitza |> dplyr::arrange(start)
gallup_ghitza |> dplyr::arrange(desc(start))

# combined data set has approve data from Roosevelt (July 1941) up to end of Biden's (Jan 2025). 
# Note: Roosevelt pres approval data from APP doesn't seem reliable, but it's hard to verify
gallup_combined |> dplyr::arrange(start)
gallup_combined |> dplyr::arrange(desc(start))

# Roper Trends appears to have data from Truman (June 1945) up to end of Trump's
# first term only (Jan 2021).
roper_trends |> dplyr::arrange(start)
roper_trends |> dplyr::arrange(desc(start))

# Roper topline data has from Roosevelt (Aug 1938) up to Trump current 2nd term (March 2025). This is the most recent. 
roper_toplines |> dplyr::arrange(start)
roper_toplines |> dplyr::arrange(desc(start))


# Ghitza et al (2022) has a column referencing the particular file name of which the data supposedly is sourced. However, it is not clear whether the authors generated these .dat files, or if such files were sourced from elsewhere (e.g., Gallup, Roper Center). 
#what files are the data sourced from?
gallup_ghitza |> dplyr::distinct(file)


# how much data per president?
gallup_ghitza |> group_by(president) |> count()
gallup_combined |> group_by(president) |> count()
roper_trends |> group_by(president) |> count()
roper_toplines |> group_by(president) |> count()


# how much data per president for each data set?
gallup_ghitza |>
  filter(president != "Roosevelt") |>
  group_by(president) |>
  summarise(ghitza_n = n(), ) |>
  mutate(
    gallup_n = gallup_site |>
      filter(president != "Trump", president != "Biden") |>
      group_by(president) |> count() |> pull(n),
    APP_n = gallup_app |>
      filter(
        president != "Trump",
        president != "Biden",
        president != "Roosevelt"
      ) |>
      group_by(president) |> count() |> pull(n)
  )

# So it looks like the number of observations per president are pretty much identical, save for a single deviation here and there (i.e., Nixon, Johnson, Ford, Bush Jr). 

# The issue with the Ghitza Gallup data is that it cannot be confirmed. Even though Ghitza et al. (2022) provide citation sources to Roper iPoll data sets, not all of these data sets were available via Roper Center's iPoll database (at least 1 for sure). More importantly, the data sets found in the source citations do not all include the presidential approval ratings. That is, some data sets did include approval ratings response data, but most others did not, and there were not enough citations to the Roper Center's iPoll database to account for all the approval ratings in their data set. It appears that the authors relied on the data available on the historical statistics and trends site page found on the Gallup website. Although they cite to the Gallup organization's presidential approval ratings site page and provide a URL link, the link is no longer a working URL, as it simply re-directs to the current working page presenting the historical approval ratings data. However in the article published in AJPS, the authors state that their data is sourced from The Roper Center's iPoll database, at least partially. It isn't clear how much of their presidential approval ratings data comes from the iPoll database.

# The way I generated the `gallup_site` (combined with the APP data) data: I simply downloaded the data available for each president available listed on that page at [](https://news.gallup.com/poll/116677/presidential-approval-ratings-gallup-historical-statistics-trends.aspx). On that page, there are time series graphs of approval ratings for most, but not all, presidents. At the bottom of each graph there was a "Get the data" hyperlink, which when clicked, downloaded a .csv file of the data presented. I downloaded the data for each president presented and compiled the data into a single data frame `gallup_site`.

# None of the approval data from alternate sources (other than Roper) provide any Question reference IDs or Roper archive numbers, or any other source identification code for each row of data. I was only able to obtain Roper archive numbers and question reference IDs from data I obtained from The Roper Center's iPoll data base. As such, This is the data I shall rely on, and only out of my interest will I attempt to match it with the data gathered from alternate sources. 

################################################################################

