# this script merges the 2024 ANES panel with the 2020 and 2016 ANES survey data
# using the respective case IDs.

# The goal is to combine each separate ANES data set in order to examine participant responses over time. This means I need to retain the data of interest for each year for the panel respondents.

# The panel sample from the 2024 ANES data (n = 2,070) reflects the number of panel respondents who completed both the pre- and post-election surveys in each of the three most recent ANES surveys. So starting with the 2024 ANES dataset, the variables from 2020 and 2016 are added to all of the cases (panel respondents). Since the ANES variable naming scheme embeds information in the variable column name itself, I can observe how an individual responded to a particular survey item presented/asked each year.

# "There are two ways it can be useful to merge data files. Merging can yield a
# data file with more respondents (cases) for a given set of variables, or with
# more variables for a given set of respondents. Or, it can yield both of these
# at once."

# So the end result is that I'll have more variables for a given set of
# respondents (panel respondents).

# NOTE: Census API key required -------------------------------------------

# This script makes use of the `censusapi` R package which relies on the Census
# API to export data. A Census API Key is required.

# If you don't have a Census API key, get one from U.S. Census Bureau site
# https://api.census.gov/data/key_signup.html
# then store the Census API key in the R environment instead of directly within
# the code. Run the code below, replacing with your API key
# Sys.setenv(CENSUS_API_KEY = "YOUR_API_KEY_HERE")

# Once you have Census API key, restart R and run the following
# Sys.getenv("CENSUS_API_KEY")


# set up ------------------------------------------------------------------

# set seed
set.seed(12345)

# load some custom functions
source(here::here('utils', 'funs.R'))

# packages
# library(tidyverse)
# library(censusapi)
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data

# read files into environment (load data) -------------------------------

# assign list of file paths and file names
anes_files <- list.files("data-raw/anes", pattern = "\\.sav$", full.names = T)
anes_files <- anes_files[2:4] # exclude unnecessary ANES data

# import/read in all ANES data simultaneously
anes_all <- purrr::map(anes_files, haven::read_sav)

# apply names to datasets in the list
names(anes_all) <- c("anes2016", "anes2020", "anes2024")

# extract data frames from list to env
# list2env(anes_all, envir = globalenv())

# Optional
# this gets rid of the the annoying prefix embedded in the value labels of ANES
# variables, e.g., "1. Response"
# If no `.col` are specified, then applies to the whole data set
anes_all <- purrr::map(
  anes_all,
  ~ labelled::update_value_labels_with(
    ., 
    .fn = ~ stringr::str_remove(., "^-*[0-9]+[\\.]\\s*")
    )
  )

# filter the data frames ---------------------------------------------------

# NOTE: I have to filter the data this way prior to merging the panel data
# together because combining the raw ANES data sets prior to filtering the data
# wasn't feasible due to the size. I tried it and it just hangs

# First I filter the 2024 and 2020 ANES data to only those who participated in
# the panel and only those who completed the pre- and post-election surveys.
anes_all$anes2024 <- anes_all$anes2024 |> dplyr::filter(V240003 == 1 & V240002c == 2) 
anes_all$anes2020 <- anes_all$anes2020 |> dplyr::filter(V200003 == 2 & V200004 == 3)

# I also filter the 2016 ANES data to include only those who completed pre- and post-election interviews
anes_all$anes2016 <- anes_all$anes2016 |> dplyr::filter(V160502 == 1)

# drop columns that are completely empty (all NA) in 2020 and 2024 data
anes_all$anes2024 <- anes_all$anes2024 |> dplyr::select(-dplyr::where(~all(is.na(.))))
anes_all$anes2020 <- anes_all$anes2020 |> dplyr::select(-dplyr::where(~all(is.na(.))))

# also zap value label `V160001_orig` where `-1` = "Inapplicable, R did not
# participate in 2016 Time Series Study". Otherwise this value label for `-1`
# conflicts with the same variable in the 2016 ANES data set
anes_all$anes2024 <- anes_all$anes2024 |> labelled::set_value_labels(V160001_orig = NULL)
  
# anes2016 sample n = 3,648
# anes2020 panel sample n = 2,670
# anes2024 panel sample n = 2,070

# combine 2016, 2020, 2024 ANES data sets ------------------------------------

# Here I join by the 2016 Time Series case ID and filter to include only those
# who completed all three survey waves. This variable is present in each data
# set and identifies the respondents who participated in the panel

# V160001_orig = 2016 Time Series Case ID
# V200001 = 2020 Time Series Case ID
# V240001 = 2024 Time Series Case ID

# merge the data sets using `purrr::reduce` and `dplyr::full_join`
# This adds the 2020 and 2024 variables to the 3,648 cases from 2016
anes_panel <- anes_all |> 
  purrr::reduce(dplyr::full_join, by = "V160001_orig")

# when variable names from two data sets are the same, a suffix of `.x` and `.y`
# is added to the column name in order to distinguish the columns. See
# anes_panel |> dplyr::select(version, dplyr::contains(".x"), dplyr::contains(".y"))

# I drop the `version` columns and merge the two `V200001` columns into one.
anes_panel <- anes_panel |> 
  dplyr::select(-dplyr::contains("version"),
                -V200001.y,
                V200001 = V200001.x)

# no columns are completely empty, that's good...
# anes_panel |> dplyr::select(dplyr::where(~all(is.na(.))))

# are any case ID variables are now gone? No
# the keyword feature of `look_for` searches the names and variable labels
# anes_panel |> labelled::look_for_and_select("Case ID")

# rename selection of variables -------------------------------------------

# Optional:
# I want the variable labels to include the original ANES variable name
# This will help me identify whether the named variable column corresponds to
# the ANES var I think it does.

# paste the ANES variable name to the variable label
anes_panel <- anes_panel |>
  labelled::update_variable_labels_with( 
    .fn = \(x) paste(names(x), x,  sep = ": "))

# check it
# anes_panel |> var_label_tab()
# viola.

# assign common variables to a named list
common_vars <- list(
  caseid2024         = "V240001",
  caseid2020         = "V200001",
  caseid2016         = "V160001_orig",
  wt_2016            = "V160102",
  strata_2016        = "V160201",
  psu_2016           = "V160202",
  wt_2020            = "V200011b",
  strata_2020        = "V200011c",
  psu_2020           = "V200011d",
  wt_2024            = "V240106b",
  psu_2024           = "V240106c",
  strata_2024        = "V240106d",
  sample_type_2020   = "V200003",
  sample_type_2024   = "V240003",
  casecomplete_      =  c("V160502",  "V200004",  "V240002c"  ),
  intv_mode_         =  c("V160501",  "V200002",  "V240002b"  ),
  st_abb_            =  c("V163001b", "V203001",  "V243001"   ),
  st_fips_           =  c("V163001a", "V203000",  "V243002"   ),
  congdist_          =  c("V161010f", "V203002",  "V243009"   ),
  census_region_     =  c("V163003",  "V203003",  "V243007"   ),
  age_               =  c("V161267",  "V201507x", "V241458x"  ),
  sex_2020           =  "V201600",
  sex_2024           =  "V241550",
  gender_2016        =  "V161342",
  gender_2024        =  "V241551",
  race_              =  c("V161310x", "V201549x", "V241501x"  ),
  hisp_              =  c("V161319x", "V201558x", "V241512x"  ),
  educ_              =  c("V161270",  "V201510",  "V241463"   ),
  educ5_2020         =  "V201511x",
  educ5_2024         =  "V241465x",
  parentsnative_     =  c("V161315",  "V201553",  "V241506"   ),
  birthplace_        =  c("V161316",  "V201554",  "V241507"   ),
  partyid_           =  c("V161158x", "V201231x", "V241227x"  ),  
  ideo_              =  c("V162171",  "V201200",  "V241177"   ),
  ideo_lean_         =  c("V162171a", "V201201",  "V241178"   ),
  income_            =  c("V162309x", "V202468x", "V241566x"  ),  
  nocare_            =  c("V162215",  "V202212",  "V242200"   ),
  nosay_             =  c("V162216",  "V202213",  "V242201"   ),
  complex_           =  c("V162217",  "V202214",  "V242202"   ),
  understand_        =  c("V162218",  "V202215",  "V242203"   ),
  voterconf_         =  c("V162219",  "V202219",  "V242207"   ),
  trustgov1_         =  c("V161215",  "V201233",  "V241229"   ),
  trustgov2_         =  c("V161216",  "V201234",  "V241231"   ),
  trustgov3_         =  c("V161217",  "V201235",  "V241232"   ),
  trustgov4_         =  c("V161218",  "V201236",  "V241233"   ),
  govresp_           =  c("V161220",  "V201238",  "V241235"   ),
  voted_             =  c("V162065x", "V202109x", "V242095x"  ),  
  presvote_          =  c("V162062x", "V202110x", "V242096x"  ),  
  houseparty_        =  c("V162067x", "V202111x", "V242097x"  ),  
  senateparty_       =  c("V162068x", "V202112x", "V242098x"  ),  
  govnrparty_        =  c("V162069x", "V202113x", "V242099x"  )
)

# I saved these to a list but when this list is unlisted, it becomes a named
# character vector. The names that are assigned to multiple variables have a
# suffix number appended to the variable name. uncomment and run the following
# to see what I mean
# names(unlist(common_vars))

# rename the variables corresponding to any variable names in the named
# character vector
anes_panel <- anes_panel |> 
  dplyr::rename(dplyr::any_of(unlist(common_vars)))

# add the ANES year as a suffix to the named variables while leaving the other
# variable names as is.
anes_panel <- anes_panel |> 
  dplyr::rename_with(
    .cols = -dplyr::matches("^V16|^V20|^V24"),
    .fn = \(x) stringr::str_replace_all(x, pattern = "\\_1$", replacement = "\\_2016")) |> 
  dplyr::rename_with(
    .cols = -dplyr::matches("^V16|^V20|^V24"),
    .fn = \(x) stringr::str_replace_all(x, pattern = "\\_2$", replacement = "\\_2020")) |>
  dplyr::rename_with(
    .cols = -dplyr::matches("^V16|^V20|^V24"),
    .fn = \(x) stringr::str_replace_all(x, pattern = "\\_3$", replacement = "\\_2024"))

# optional: reorganize column positions in data set
anes_panel <- anes_panel |> 
  dplyr::select(
    dplyr::contains("caseid"),
    dplyr::contains("wt_"),
    dplyr::contains("strata_"),
    dplyr::contains("psu_"), 
    !dplyr::matches("^V16|^V20|^V24"),
    dplyr::matches("^V16|^V20|^V24")
    )

# filter data to include only those respondents who participated in all three
# ANES surveys
anes_panel <- anes_panel |> dplyr::filter(casecomplete_2024 == 2)
  
# equivalent
# anes_panel |> dplyr::filter(!is.na(caseid2024))

# generate codebook
anes_panel_codebook <- codebook(anes_panel)

# add `rowid` column
anes_panel <- anes_panel |> tibble::rowid_to_column()


# save --------------------------------------------------------------------

# save ANES 2016-2020-2024 panel data as .rds
readr::write_rds(anes_panel, file = "data/anes data/anes_panel_2016_2020_2024.rds")

# save codebook
readr::write_csv(
  anes_panel_codebook, 
  file = "resources/anes resources/anes_panel_2016_2020_2024_codebook.csv"
  )


# clear envir/memory
rm(list = ls())
