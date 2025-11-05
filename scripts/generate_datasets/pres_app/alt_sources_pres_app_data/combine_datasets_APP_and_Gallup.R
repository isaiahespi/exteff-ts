# Here I am creating a dataset consisting of data derived from the American Presidency Project at UC Santa Barbara and data gleaned from Gallup Historical Statistics and Trends of Presidential Approval ratings page

# The APP data presents presidential approval ratings up to 2022-01-06, whereas the the Gallup derived data presents approval ratings up to the end of Biden's term in office (2025-01-15) but does not include any approval data for Roosevelt. Note that the Gallup data only includes percentage proportions of "approve" responses, so "disapprove" and "unsure/no data" are no included. Also note that for Obama and Trump, the Gallup data pertains to weekly averages, which means there were no "start" and "end" dates provided for when the surveys were conducted. Why did Gallup do weekly averages for Obama and Trump? I don't know.

# The goal here was to see if the data from the Gallup site matched the data presented by the APP at UC Santa Barbara. Since the data is supposedly from the same source (Gallup) then it all should match save for the missing or omitted data from either data set. 

# If the data sets sufficiently match where possible, then they can be combined into one comprehensive data frame of presidential approval ratings data.

# set seed for reproducibility
set.seed(12345)

# load package
library(tidyverse)

# load in approval from APP UCSB
american_pres_proj <- readr::read_rds("data/american_pres_proj_UCSantaBarbara_presidential_app_data.rds")

# load in approval data gleaned from Gallup historical statistics and trends page
gallup_pres <- readr::read_rds("data/gallup_pres_app_historical_stats_trends.rds")

# Does the Gallup data pulled from the trends site match APP data? :::::::::####

# oddly, the data pulled from the Gallup page has more observations than the data from the APP UCSB page. This is likely due to the fact that APP doesn't have data beyond January 2022, and the Gallup data doesn't have approval ratings for Roosevelt. So the Gallup data has 1890 rows of data, whereas APP has 1869 rows of data

# filter out years beyond 2021 and Roosevelt
american_pres_proj |> filter(year <= 2021 & president != "Roosevelt")

# filter out years beyond 2021
gallup_pres |> filter(year <= 2021)

# When both data sets are constrained to only include data from Truman up to the
# end of 2021, APP n = 1,849, whereas Gallup n = 1,852. A difference of only 3
# observations. So before comparing, I know for sure that at least 3 rows won't
# match.

# There are only 37 rows on Biden contained in Gallup that are not included in APP,
gallup_pres |> filter(president == "Biden" & year >=2022) |> count()-1

# and only 20 rows on Roosevelt in APP that are not in Gallup. 
american_pres_proj |> filter(president == "Roosevelt") |> count()

# So those missing rows combined only account for 57 rows.

# Additionally, I know that there are no survey "end" dates in the Gallup data for Obama and Trump. The total missing an end date is 559. 
gallup_pres |> filter(is.na(end)) |> count()

# So all that means is that when the data sets are compared, they can only be matched on the columns `president`, `year`, `month`, `start`, and `approve`. I thus would expect there to be somewhere around 1,849

# Do 1,849 rows match between data sets? :::::::::::::::::::::::::::::::::::####

# An inner_join() only keeps observations from x that have a matching key in y.
# The most important property of an inner join is that unmatched rows in either
# input are not included in the result. Most of the time this is inappropriate
# as observations are easily lost.

# this automatically omits Roosevelt and non-matching Biden rows since it
# returns all matched x rows
american_pres_proj |>  
  dplyr::inner_join(
    gallup_pres, 
    by = dplyr::join_by(president, year, month, start, approve), 
    relationship = "one-to-one" # specify expected relationship between rows
    ) |> 
  arrange(start)

# So only 1,841 rows match between data sets when based on the columns specified in the function. That's less than expected but not by much.

# same result
# return rows in x with a match in y
gallup_pres |>
  dplyr::semi_join(american_pres_proj, by = dplyr::join_by(start, approve))

# 29 rows in APP don't find a match in Gallup data (1870-29 = 1841)
# When Roosevelt is excluded, there at 9 rows in APP without a match in Gallup
# rows in x without a match in y
american_pres_proj |> 
  dplyr::filter(president != "Roosevelt") |> 
  dplyr::anti_join(gallup_pres, by = dplyr::join_by(president, start, approve))

# 49 rows in Gallup data don't find a match in APP (1890-49 = 1841) 
# rows in x without a match in y
gallup_pres |>
  dplyr::anti_join(
    american_pres_proj, 
    by = dplyr::join_by(president, start, approve)) |> 
  print(n = Inf)

# when Biden is filtered out, there are 12 rows in Gallup without a match in
# APP.
gallup_pres |>
  dplyr::filter(president != "Biden") |> 
  dplyr::anti_join(
    american_pres_proj, 
    by = dplyr::join_by(president, start, approve)) |> 
  print(n = Inf)


# Examine where and how different the differences are between data sets ::::####

# full join data sets by president, year, month, and start date of survey
# filter to display rows where `approve` in Gallup data doesn't equal `approve`
# in APP data
gallup_pres |>  
  # returns all x rows, followed by unmatched y rows.
  dplyr::full_join(
    american_pres_proj, 
    by = dplyr::join_by(president, year, month, start), 
    relationship = "one-to-one") |>
  mutate(
    end = dplyr::coalesce(end.x, end.y), 
    approve = dplyr::coalesce( approve.y, approve.x),
    .after = start) |> 
  filter(approve.x != approve.y)
# approval rating for 6 rows differ between data sets. Approval ratings differ by 1 except for one instance where the difference between approval ratings is 4 percentage points.


# full join data sets by president, year, month, and start date of survey
# filter to display rows where `end` in Gallup data doesn't equal `end`
# in APP data
gallup_pres |>  
  # returns all x rows, followed by unmatched y rows.
  dplyr::full_join(
    american_pres_proj, 
    by = dplyr::join_by(president, year, month, start), 
    relationship = "one-to-one") |>
  mutate(
    end = dplyr::coalesce(end.x, end.y), 
    approve = dplyr::coalesce( approve.y, approve.x),
    .after = start) |> 
  filter(end.x != end.y)
# two end dates differ


# full join data sets by president, year, month, and end date of survey
# filter to display rows where `start` in Gallup data doesn't equal `start`
# in APP data
gallup_pres |>  
  # returns all x rows, followed by unmatched y rows.
  dplyr::full_join(
    american_pres_proj, 
    by = dplyr::join_by(president, year, month, end)
    ) |>
  mutate(
    start = dplyr::coalesce(start.x, start.y), 
    approve = dplyr::coalesce( approve.y, approve.x),
    .before = end) |> 
  filter(start.x != start.y)
# Two start dates differ


# Examining some of the differences between non-matching rows reveals that most of the differences are minor deviations in the starting or end dates, or in the "approve" rating. In some cases, the approval rating found in the APP data differs by 1 percentage point from the approval ratings in the Gallup data (save for one instance where there is a 4 percentage point difference), even though the dates are identical. In some other cases, dates differ by a day or more but approval ratings are identical. Since I am unable to verify the accuracy of either, the best approach is to ensure that for all non-matching cases, the data derived from the Gallup site takes precedence as far as the start and end dates are concerned, whereas the APP ratings data will take precedence since approve, disapprove, and unsure are all included and sum to 100. (Note that only the approval ratings for Roosevelt in the APP data set do not sum to 100. Who knows why)

# Combine data sets ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# I want to simply combine all observations from both data frames without
# duplicating any. So the data should include observations from Roosevelt up to
# the end of Biden's term. So there should be 1,910 rows in the
# combined data frame at least.


# the following composes a dataframe consisting of data derived from the American Presidency Project at UC Santa Barbara and data gleaned from Gallup Historical Statistics and Trends of Presidential Approval ratings page. The Gallup data start and end dates take precedence (i.e., will overwrite the APP data upon differences), whereas the APP approval ratings data takes precedence.
pres_app <- gallup_pres |>  
  # returns all x rows, followed by unmatched y rows.
  dplyr::full_join(
    american_pres_proj, 
    by = dplyr::join_by(president, year, month, start), 
    relationship = "one-to-one") |>
  mutate(
    end = dplyr::coalesce(end.x, end.y), 
    approve = dplyr::coalesce( approve.y, approve.x),
    .keep = "unused", 
    .after = start)

  
# save as .rds file
readr::write_rds(pres_app, file = "data/presidential_approval_gallup.rds")

################################################################################
# Unless I contact the Gallup Organization or purchase presidential approval
# rating data myself, the other potential method for verifying the data here is
# to locate the specific Gallup Polls on Roper iPOLL. 


