# A recently published paper in AJPS (Ghitza, Gelman, and Auerbach 2023) included time series presidential approval ratings data from Gallup sourced from the Roper Center's iPoll database. 

# I downloaded the replication data for the paper found in the Harvard Dataverse at the URL below: # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TZVDUQ

# See citation to paper:

# Ghitza, Yair, Andrew Gelman, and Jonathan Auerbach. 2023. “The Great Society, Reagan’s Revolution, and Generations of Presidential Voting.” American Journal of Political Science 67(3): 520–37. doi:10.1111/ajps.12713.

# I do the following in the code that follows
# import the Gallup data, 
# rename a select few columns, 
# exclude partisan approval ratings data (i.e., `dem`, `ind`, `rep`), 
# recode "Bush I" and "Bush II" to "Bush" and "BushJr", respectively, 
# add distinct columns for year and month derived from the start date of the poll data.

# my intention is to compare this data from the data set I managed to pull from the Roper Center's iPoll database myself. Although my ultimate goal is not to fully replicate the data set, I am to approximate as close a match as possible given the source for both is supposedly the same.

# load packages
library(tidyverse)
# library(pdftools)
# library(ropercenter)

# load in the Gallup approval data from Ghitza replication data
ghitza <- readRDS("~/R/replications/ghitza2023/data-raw/gallup_approval_with_2016.rds") |> 
  dplyr::as_tibble() |> 
  dplyr::rename(
    president = pres, approve = app, disapprove = dis, unsure = und
  ) |> 
  # exclude the partisan approval ratings.
  dplyr::select(-dem, -ind, -rep)

# rename the presidents to match what I have (Bush and BushJr)
ghitza <- ghitza |> 
  dplyr::mutate(
    president = dplyr::case_when(
      president == "Bush I" ~ "Bush",
      president == "Bush II" ~ "BushJr",
      .default = as.character(president)
    )
  )

# add columns for year and month to ghitza data
ghitza <- ghitza |>  
  mutate(year = lubridate::year(start),
         month = lubridate::month(start,label = T, abbr = T), 
         .before = start)


# save Ghitza et al. (2023) Gallup presidential approval ratings data as .rds
readr::write_rds(ghitza, file = "data/gallup_pres_approval_Ghitza_et_al_2023.rds")



