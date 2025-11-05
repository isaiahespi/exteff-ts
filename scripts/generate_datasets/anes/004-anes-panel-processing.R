# this script "processes" the merged 2016-2020-2024 ANES panel data. 

# I create different variables (e.g., age_cat), remove unnecessary variables,
# create indices, and create factor variables of select named variable columns.

# I don't remove any of the variables from the original ANES data sets.

# The next step after this script is to create a survey design object from the
# panel survey data

# set up ------------------------------------------------------------------

# set seed for reproducibility
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

# import/load in ANES 2016-2020-2024 panel data
anes_panel <- readr::read_rds("data/anes data/anes_panel_2016_2020_2024.rds") |> 
  haven::zap_formats()


# This dataset includes survey data for the panel of respondents (cases) who
# participated in the post-election interviews across the previous three ANES
# surveys (n = 2,070). Every variable from each survey has been retained,
# although a smaller selection of variables been renamed for ease of use.

# import ANES Panel data codebook for reference
anes_panel_codebook <- readr::read_csv(
  file = "resources/anes resources/anes_panel_2016_2020_2024_codebook.csv")

# this codebook contains the variable names and labels for every variable
# included in the 2016, 2020, and 2024 ANES surveys. The variable labels have
# been prefixed with the original variable code from each ANES survey for ease
# of reference.


# processing --------------------------------------------------------------

# create age group (`age_cat`) variable -----------------------------------

# anes_panel |> 
#   dplyr::select(dplyr::contains("age"), V161267c, V161267x) |>
#   var_label_tab()

# Age of the panel shouldn't match between elections of course. Only their age
# in 2024 will matter. I'll retain the other age variables for posterity.

# create age_cat factor variable and set variable label
anes_panel <- anes_panel |> 
  dplyr::mutate(age_cat = cut(
    x = age_2024,
    c(17, 29, 39, 49, 59, 69, 79, 200),
    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 or older")),
    .after = age_2024) |> 
  sjlabelled::var_labels(age_cat = "Age group")


# determine proportion of 'don't know' responses --------------------------

## determine proportion and counts of 'Don't know' responses to efficacy items
## for each year

efficacy_items <- anes_panel |>
  dplyr::select(dplyr::contains("nocare"), dplyr::contains("nosay")) |>
  colnames() |>
  dput()

# freaks(dat = anes_panel, vars = dplyr::all_of(efficacy_items))

# So only one person in 2016 responded with "don't know" to the NOCARE item. I'd
# say its' safe to recode all negative values as NA

# code missing values as NA -------------------------------------------------------


# Missing data are coded as negative integers, e.g., "-8 = Don't know".
# this is the case across all three ANES survey data sets.

# set negative values as NA for all variables in the data set
anes_panel <- anes_panel |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~sjlabelled::set_na(., na = c(-9,-8, -7, -6, -5, -4, -3, -2, -1))
      )
    )


# remove unnecessary weight, strata, and psu vars -------------------------

# the only weight, strata, and psu variables that matter for this panel are the
# ones from 2024.

# remove the unnecessary weight, strata, and PSU variables from the 2020 and 2016 data
anes_panel <- anes_panel |>
  # rename 2024 weight, strata, psu variables
  dplyr::rename(wt_panel = wt_2024, strata = strata_2024, psu = psu_2024) |> 
  # remove unnecessary variables (note the minus sign excludes these columns)
  dplyr::select(-dplyr::matches("^wt_[0-9]"), 
                -dplyr::matches("^strata_[0-9]"),
                -dplyr::matches("^psu_[0-9]")
                )



# create recoded variables of NOCARE and NOSAY following ANES CDF  --------


# anes_panel |> 
#   dplyr::select(dplyr::contains("nocare"), dplyr::contains("nosay")) |>
#   purrr::map(sjlabelled::get_labels, values = 'p')


# create an external efficacy index following the coding scheme of the
# American National Election Studies (ANES).

# create recoded variables of NOCARE and NOSAY following ANES CDF coding
# scheme
anes_panel <- anes_panel |>
  dplyr::mutate(dplyr::across(
    dplyr::all_of(efficacy_items),
    ~ labelled::labelled(
      dplyr::case_when(.x %in% c(1, 2) ~ 0, .x == 3 ~ 50, .x %in% c(4, 5) ~ 100, TRUE ~ NA),
      labels = c(
        "Agree" = 0,
        "Neither agree nor disagree" = 50,
        "Disagree" = 100
      ),
      label = paste(
        attr(.x, "label"),
        " values recoded to match ANES CDF coding scheme",
        sep = ","
      )
    ),
    .names = "{col}.cdf"
  ))

# confirm
# anes_panel |> 
#   dplyr::select(dplyr::contains(".cdf")) |>
#   purrr::map(sjlabelled::get_labels, values = 'p')

# create external efficacy index following ANES CDF coding -------------------

# create unipolar external efficacy index variable matching ANES CDF external
# efficacy variable (VCF0648)
# NOTE: this is different from the aggregate average value of the index. This
# variable reflects an individual's 'score' on the index

anes_panel <- anes_panel |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    exteff2016.cdf = labelled::labelled(mean(c(nocare_2016.cdf, nosay_2016.cdf), na.rm = T), 
                                        label = "2016 External Efficacy Index, ANES CDF coded"),
    exteff2020.cdf = labelled::labelled(mean(c(nocare_2020.cdf, nosay_2020.cdf), na.rm = T),
                                        label = "2020 External Efficacy Index, ANES CDF coded"),
    exteff2024.cdf = labelled::labelled(mean(c(nocare_2024.cdf, nosay_2024.cdf), na.rm = T),
                                        label = "2024 External Efficacy Index, ANES CDF coded")
    ) |> 
  dplyr::mutate(
    exteff2016.cdf = dplyr::case_when(
      is.na(nocare_2016.cdf) & is.na(nosay_2016.cdf) ~ NA, .default = exteff2016.cdf),
    exteff2020.cdf = dplyr::case_when(
      is.na(nocare_2020.cdf) & is.na(nosay_2020.cdf) ~ NA, .default = exteff2020.cdf),
    exteff2024.cdf = dplyr::case_when(
      is.na(nocare_2024.cdf) & is.na(nosay_2024.cdf) ~ NA, .default = exteff2024.cdf),
    ) |> 
  dplyr::ungroup()


# anes_panel |>
#   dplyr::select(dplyr::matches("exteff")) |>
#   var_label_tab()


exteff_cdf_vars <- anes_panel |>
  dplyr::select(dplyr::matches("exteff[0-9]{4}.cdf")) |> colnames() |> dput()

# create recoded versions of external efficacy component items --------

# I aim to create an external political efficacy index that remains bipolar and
# symmetric around a neutral point. Negative values reflect less external
# efficacy (or rather, the opposing attitude of 'efficacy'), positive values
# reflect positive external efficacy, and the middling zero point reflects an
# insecure neutral position. This corresponds to the symmetrical response option
# format of the two external efficacy items in which respondents select one of
# five options to express the extent to which they agree, disagree, or whether
# they neither agree nor disagree with the given item statement.

# In addition, 

# first, I recode the two items so that each ranges from -1 to 1. Specifically,
# "Agree strongly" = -1, "Agree somewhat" = -0.5, "Neither agree nor disagree" =
# 0, "Disagree somewhat" = 0.5, "Disagree strongly" = 1.
anes_panel <- anes_panel |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(efficacy_items),
      ~labelled::labelled(
        dplyr::case_when(
          .x == 1 ~ -1,
          .x == 2 ~ -0.5,
          .x == 3 ~ 0,
          .x == 4 ~ 0.5,
          .x == 5 ~ 1,
          TRUE ~ NA), 
        labels = c("Agree strongly" = -1,
                   "Agree somewhat" = -0.5,
                   "Neither agree nor disagree" = 0,
                   "Disagree somewhat" = 0.5,
                   "Disagree strongly" = 1), 
        label = paste(attr(.x, "label"), "bipolar and symmetric coding", sep = ",")
          ),
      .names = "{col}.recode")
    )

# confirm
# anes_panel |> 
#   dplyr::select(dplyr::contains(".recode")) |>
#   sjlabelled::get_labels()

# freaks(dat = anes_panel, vars = dplyr::contains(".recode"))

# create external efficacy index from recoded component items -------------

# second, I create an external efficacy variable per respondent which is
# composed of the combination of responses to both external efficacy item
# responses. The responses are combined and summed to reflect a raw value/score
# of external efficacy on a 9-point range from -1 to 1.

anes_panel <- anes_panel |> 
  dplyr::rowwise() |> 
  # mean of sum = sum(x1, x2)/valid_n
  dplyr::mutate(
    exteff2016.rec = labelled::labelled(mean(c(nocare_2016.recode, nosay_2016.recode), na.rm = T),
                                        label = "2016 External Efficacy, bipolar scale"),
    exteff2020.rec = labelled::labelled(mean(c(nocare_2020.recode, nosay_2020.recode), na.rm = T),
                                        label = "2020 External Efficacy, bipolar scale"),
    exteff2024.rec = labelled::labelled(mean(c(nocare_2024.recode, nosay_2024.recode), na.rm = T),
                                        label = "2024 External Efficacy, bipolar scale"),
                ) |> 
  dplyr::mutate(
    exteff2016.rec = dplyr::case_when(
      is.na(nocare_2016.recode) & is.na(nosay_2016.recode) ~ NA, .default = exteff2016.rec),
    exteff2020.rec = dplyr::case_when(
      is.na(nocare_2020.recode) & is.na(nosay_2020.recode) ~ NA, .default = exteff2020.rec),
    exteff2024.rec = dplyr::case_when(
      is.na(nocare_2024.recode) & is.na(nosay_2024.recode) ~ NA, .default = exteff2024.rec),
    ) |> 
  dplyr::ungroup()

# anes_panel |>
#   dplyr::select(dplyr::matches("exteff")) |>
#   var_label_tab()

# create character vector of only recoded external efficacy index variables
exteff_rec_vars <- anes_panel |>
  dplyr::select(dplyr::matches("exteff[0-9]{4}.rec")) |>
  colnames() |>
  dput()




# create recoded trust in government items following ANES CDF coding ---------

# have to do this piece-meal because the trust-in-government items have
# different response options with different value codes.

trustgov1_items <- anes_panel |> 
  dplyr::select(dplyr::contains("trustgov1")) |> colnames() |> dput()

trustgov2_items <- anes_panel |> 
  dplyr::select(dplyr::contains("trustgov2")) |> colnames() |> dput()

trustgov3_items <- anes_panel |> 
  dplyr::select(dplyr::contains("trustgov3")) |> colnames() |> dput()

trustgov4_items <- anes_panel |> 
  dplyr::select(dplyr::contains("trustgov4")) |> colnames() |> dput()


# create recoded trust in gov items following the ANES CDF coding scheme 
anes_panel <- anes_panel |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(trustgov1_items),
      ~ labelled::labelled(
        dplyr::case_when(
          .x %in% c(5, 4) ~ 0, 
          .x == 3 ~ 33, 
          .x == 2 ~ 67, 
          .x == 1 ~ 100, 
          .default = .x),
        label = paste(attr(.x, "label"), " values recoded to match ANES CDF coding scheme",
                      sep = ",")),
      .names = "{col}.cdf"
    ),
    
    dplyr::across(
      dplyr::all_of(trustgov2_items),
      ~ labelled::labelled(
        dplyr::case_when(
          .x == 1 ~ 0, 
          .x == 2 ~ 100, 
          .default = .x),
        label = paste(attr(.x, "label"), " values recoded to match ANES CDF coding scheme", sep = ",")),
      .names = "{col}.cdf"
    ),
    
    dplyr::across(
      dplyr::all_of(trustgov3_items),
      ~ labelled::labelled(
        dplyr::case_when(
          .x == 1 ~ 0, 
          .x == 2 ~ 50, 
          .x == 3 ~ 100, 
          .default = .x),
        label = paste(attr(.x, "label"), " values recoded to match ANES CDF coding scheme", sep = ",")),
      .names = "{col}.cdf"
    ),
    
    dplyr::across(
      dplyr::all_of(trustgov4_items),
      ~ labelled::labelled(
        dplyr::case_when(
          .x %in% c(5, 4) ~ 100, 
          .x == 3 ~ 50, 
          .x %in% c(2, 1) ~ 0, 
          .default = .x),
        label = paste(attr(.x, "label"), " values recoded to match ANES CDF coding scheme", sep = ",")),
      .names = "{col}.cdf"
    )
  )



# confirm
# anes_panel |> 
#   dplyr::select(dplyr::matches("trustgov") & dplyr::ends_with(".cdf")) |>
#   var_label_tab()


# create trust in government index variable -------------------------------


anes_panel <- anes_panel |>
  dplyr::mutate(
    trustgov2016.cdf = labelled::labelled(rowMeans(
      dplyr::pick(
        trustgov1_2016.cdf,
        trustgov2_2016.cdf,
        trustgov3_2016.cdf,
        trustgov4_2016.cdf
      ),
      na.rm = T
    ), label = "2016 Trust in Government Index, ANES CDF coded"),
    trustgov2020.cdf = labelled::labelled(rowMeans(
      dplyr::pick(
        trustgov1_2020.cdf,
        trustgov2_2020.cdf,
        trustgov3_2020.cdf,
        trustgov4_2020.cdf
      ),
      na.rm = T
    ), label = "2020 Trust in Government Index, ANES CDF coded"),
    trustgov2024.cdf = labelled::labelled(rowMeans(
      dplyr::pick(
        trustgov1_2024.cdf,
        trustgov2_2024.cdf,
        trustgov3_2024.cdf,
        trustgov4_2024.cdf
      ),
      na.rm = T
    ), label = "2024 Trust in Government Index, ANES CDF coded")
  )


# anes_panel |>
#   dplyr::select(dplyr::matches("trustgov[0-9]{4}.")) |>
#   var_label_tab()

# anes_panel |>
#   dplyr::select(dplyr::matches("trustgov[0-9]{4}."))


# save indices as character vector
trustgov_indx_vars <- anes_panel |>
  dplyr::select(dplyr::matches("trustgov[0-9]{4}.")) |>
  colnames() |>
  dput()


# create `moved` variable  ------------------------------------------------

# anes_panel |> 
#   dplyr::select(dplyr::contains("st_abb"),dplyr::contains("census")) |>
#   var_label_tab()

# Given the aim of the study, it is important to consider whether the panel
# respondent relocated to a different state between elections. This might be
# even more important if the panel member relocated to a different Census region
# of the United States. Also worth considering is whether a panel member
# relocated to a different congressional district.

# create variable that indicates whether sample location state of panel case
# differed across ANES surveys.
anes_panel <- anes_panel |>
  dplyr::mutate(
    moved = labelled::labelled(
      dplyr::case_when(
        st_abb_2016 != st_abb_2020 &
          st_abb_2020 == st_abb_2024 ~ 1, # moved after 2016
        st_abb_2020 != st_abb_2024 &
          st_abb_2016 == st_abb_2020 ~ 2, # moved after 2020
        st_abb_2016 != st_abb_2020 &
          st_abb_2020 != st_abb_2024 &
          st_abb_2016 != st_abb_2024 ~ 3, # moved after every election
        st_abb_2016 != st_abb_2020 &
          st_abb_2020 != st_abb_2024 &
          st_abb_2016 == st_abb_2024 ~ 4, # moved back to 2016 state
        st_abb_2016 == st_abb_2020 &
          st_abb_2020 == st_abb_2024 &
          st_abb_2016 == st_abb_2024 ~ 0 # did not move
      ),
      labels = c(
        'Did not move' = 0,
        'Moved after 2016' = 1,
        'Moved after 2020' = 2,
        'Moved after each election' = 3,
        'Moved back to 2016 State' = 4
      ),
      label = "Indicates whether panel member's state of residence differed between electoral periods"
    )
  )

# create dummy variable of the same
anes_panel <- anes_panel |> 
  dplyr::mutate(moved.dum = labelled::labelled(
    dplyr::case_when(moved > 0 ~ 1, moved == 0 ~ 0, TRUE ~ NA),
    labels = c("Did not move" = 0, "Moved" = 1),
    label = "Dummy variable indicating whether a panel member's state location differed between electoral periods"
  )) 


# create `moved_region` variable ------------------------------------------

# create variable indicating whether census region of panel case differed
# between ANES surveys.
anes_panel <- anes_panel |>
  dplyr::mutate(
    moved_region = labelled::labelled(
      dplyr::case_when(
        census_region_2016 != census_region_2020 &
          census_region_2020 == census_region_2024 ~ 1, # diff region after 2016
        census_region_2020 != census_region_2024 &
          census_region_2016 == census_region_2020 ~ 2, # diff region after 2020
        census_region_2016 != census_region_2020 &
          census_region_2020 != census_region_2024 &
          census_region_2016 != census_region_2024 ~ 3, # diff region every election
        census_region_2016 != census_region_2020 &
          census_region_2020 != census_region_2024 &
          census_region_2016 == census_region_2024 ~ 4, # returned to 2016 region after 2020
        census_region_2016 == census_region_2020 &
          census_region_2020 == census_region_2024 &
          census_region_2016 == census_region_2024 ~ 0 # did not move
      ),
      labels = c(
        'Did not move' = 0,
        'Different region after 2016' = 1,
        'Different region 2020' = 2,
        'Different region each election' = 3,
        'Returned to 2016 region' = 4
      ),
      label = "Indicates whether Census region of panel case differed between electoral periods"
    )
  )

# create dummy variable of the same
anes_panel <- anes_panel |> 
  dplyr::mutate(moved_region.dum = labelled::labelled(
    dplyr::case_when(moved_region > 0 ~ 1, moved_region == 0 ~ 0, TRUE ~ NA),
    labels = c("Did not move" = 0, "Moved region" = 1),
    label = "Dummy variable indicating whether Census region of panel case differed between electoral periods"
  )) 



# make st_fips variables --------------------------------------------------


anes_panel <- anes_panel |>  
  dplyr::mutate(
    st_fips_2024 = labelled::labelled(as.numeric(fipio::as_fips(anes_panel$st_abb_2024)),
                                      label = paste(attr(st_fips_2024, "label"))),
    st_fips_2020 = labelled::labelled(as.numeric(fipio::as_fips(anes_panel$st_abb_2020)),
                                      label = paste(attr(st_fips_2020, "label"))),
    st_fips_2016 = labelled::labelled(as.numeric(fipio::as_fips(anes_panel$st_abb_2016)),
                                      label = paste(attr(st_fips_2016, "label"))))



# rename certain variables of interest ------------------------------------


anes_panel <- anes_panel |>
  dplyr::rename(
    yrs_at_resid2016 = V161337,
    yrs_at_resid2020 = V201587,
    yrs_at_resid024 = V241534,
    st_raised2016   = V161330,
    st_raised2020   = V201575,
    st_raised2024   = V241529
  )

# optional: reorganize dataframe
anes_panel <- anes_panel |> 
  dplyr::select(
    rowid, 
    dplyr::contains("caseid"), 
    wt_panel, 
    strata, 
    psu,
    dplyr::contains("casecomplete"),
    dplyr::contains("intv_mode_"),
    dplyr::contains("sample_type"),
    dplyr::contains("census_region"),
    dplyr::contains("st_abb"),
    dplyr::contains("st_fips"),
    dplyr::contains("congdist"),
    dplyr::contains("moved"),
    dplyr::contains("yrs_at_resid"),
    dplyr::contains("st_raised"),
    dplyr::contains("age"),
    dplyr::contains("sex_"),
    dplyr::contains("gender_"),
    dplyr::contains("race"),
    dplyr::contains("hisp"),
    dplyr::contains("parents"),
    dplyr::contains("birthplace"),
    dplyr::contains("educ_"),
    dplyr::contains("educ5_"),
    dplyr::contains("partyid_"),
    dplyr::contains("ideo_"),
    dplyr::contains("ideo_lean_"),
    dplyr::contains("income_"),
    dplyr::contains("nocare_"),
    dplyr::contains("nosay_"),
    dplyr::contains("complex_"),
    dplyr::contains("understand_"),
    dplyr::matches("exteff[0-9]{4}.cdf"),
    dplyr::matches("exteff[0-9]{4}.rec"),
    dplyr::contains("trustgov"),
    dplyr::matches("trustgov[0-9]{4}."),
    dplyr::contains("govresp"),
    dplyr::contains("voterconf"),
    dplyr::contains("voted_"),
    dplyr::contains("presvote_"),
    dplyr::contains("houseparty_"),
    dplyr::contains("senateparty_"),
    dplyr::contains("govnrparty_"),
    dplyr::matches("^V\\d"))



# make factor versions of select vars -------------------------------------

# all caseid vars should be character vectors.
anes_panel <- anes_panel |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::contains("caseid"),
      ~labelled::to_character(.)))

# anes_panel |> 
#   dplyr::select(!dplyr::matches("^V\\d")) |> 
#   dplyr::glimpse()

# convert select variables into factor, identify them with a suffix ".fct"
anes_panel <- anes_panel |>
  dplyr::mutate(dplyr::across(
    c(
    dplyr::contains("casecomplete"),
    dplyr::contains("intv_mode_"),
    dplyr::contains("sample_type"),
    dplyr::contains("census_region"),
    dplyr::contains("moved"),
    dplyr::contains("st_raised"),
    dplyr::contains("sex_"),
    dplyr::contains("gender_"),
    dplyr::contains("race"),
    dplyr::contains("hisp"),
    dplyr::contains("parents"),
    dplyr::contains("birthplace"),
    dplyr::contains("educ_"),
    dplyr::contains("educ5_"),
    dplyr::contains("partyid_"),
    dplyr::contains("ideo_"),
    dplyr::contains("ideo_lean_"),
    dplyr::matches("nocare_[0-9]{4}$"),
    dplyr::matches("nosay_[0-9]{4}$"),
    dplyr::contains("complex_"),
    dplyr::contains("understand_"),
    dplyr::matches("trustgov[0-9]{1}_[0-9]{4}$"),
    dplyr::contains("govresp"),
    dplyr::contains("voterconf"),
    dplyr::contains("voted_"),
    dplyr::contains("presvote_"),
    dplyr::contains("houseparty_"),
    dplyr::contains("senateparty_"),
    dplyr::contains("govnrparty_")
    ),
    ~ sjlabelled::as_label(.),
    .names = "{col}.fct"
  ))


# confirm
# anes_panel |> 
#   dplyr::select(dplyr::where(is.factor)) |> 
#   dplyr::glimpse()

# convert interview dates to proper date class objects --------------------


anes_panel <- anes_panel |> 
  dplyr::rename(
    preintvdate_2016   = V164002,
    preCAPIstart_2016  = V164002a,
    postintvdate_2016  = V165002,
    postCAPIstart_2016 = V165002a,
    preintvdate_2020   = V203053, 
    posttintvdate_2020 = V203078,
    preintvdate_2024   = V243050,
    postintvdate_2024  = V243060
  ) |>
  dplyr::mutate(dplyr::across(
    c(
      preintvdate_2016,
      postintvdate_2016,
      preintvdate_2020,
      posttintvdate_2020,
      preintvdate_2024,
      postintvdate_2024
    ),
    ~ lubridate::ymd(.)
  ))

anes_panel <- anes_panel |> 
  labelled::set_variable_labels(
    preintvdate_2016   = "V164002: PRE ADMIN: Beginning date of Pre IW (YYYYMMDD)",
    postintvdate_2016  = "V165002: POST ADMIN: Beginning date of Post IW (YYYYMMDD)",
    preintvdate_2020   = "V203053: PRE ADMIN: Beginning date of Pre IW (YYYYMMDD)",
    posttintvdate_2020 = "V203078: POST ADMIN: Beginning date of Post IW (YYYYMMDD)",
    preintvdate_2024   = "V243050: ADMIN: Pre-election interview start date (YYYYMMDD)",
    postintvdate_2024  = "V243060: ADMIN: Post-election interview start date (YYYYMMDD)"
    )

# make final variable label for `rowid`

anes_panel <- anes_panel |> 
  labelled::set_variable_labels(rowid = "Row ID")


# codebook ----------------------------------------------------------------

anes_panel_codebook <- codebook(anes_panel)

# save --------------------------------------------------------------------



# save ANES 2016-2020-2024 panel data as .rds
readr::write_rds(anes_panel, file = "data/anes data/anes_panel_2016_2020_2024.rds")

# save codebook
readr::write_csv(
  anes_panel_codebook, 
  file = "resources/anes resources/anes_panel_2016_2020_2024_codebook.csv"
  )

# clear environment
rm(list = ls())
