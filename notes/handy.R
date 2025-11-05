# some handy things stored here for reference later.

# this counts how many instances of `age` meet the condition `< 18`
data |> 
  dplyr::filter(!is.na(age)) |> 
  dplyr::count(age < 18)

# custom frequency table for age using `tidyr::pivot_longer`
data |>
  dplyr::summarise(
    age18_and_up = sum(age >= 18, na.rm = T),
    below18 = sum(age < 18, na.rm = T),
    age_missing = sum(is.na(age)),
    total = n(),
  ) |>
  tidyr::pivot_longer(cols = dplyr::everything())

# simple dplyr way to get counts and proportions
data |>  
  dplyr::select(cit, age, cit, attn1, attn2) |>
  dplyr::count(cit, sort = F) |> 
  dplyr::mutate(prop = n/sum(n))

# handy way to count a list of factors at once
data |> 
  dplyr::select(dplyr::contains("_vig_manip")) |>
  purrr::map(forcats::fct_count)

# quick way to see variable and label
data |>
  dplyr::select(dplyr::contains("party")) |> 
  sjlabelled::get_label() |> 
  tibble::enframe()

# Identify counts of discrepancies between `lucid_age` and 'age'
data |> 
  dplyr::select(rowid, lucid_age, age) |> 
  dplyr::mutate(match = ifelse(age == lucid_age, TRUE, FALSE)) |>
  dplyr::summarise(
    "Matches" = sum(match == TRUE, na.rm = T),
    "Non-match" = sum(match == FALSE, na.rm = T)
  )

# check for missing values among variables of interest
data |> 
  dplyr::select(trust1, dplyr::contains("distrust_"), safety, recruitpref) |>  
  dplyr::summarise_all(~sum(is.na(.))) |> 
  tidyr::pivot_longer(cols = dplyr::everything(), 
               names_to = "items", 
               values_to = "n_missing")

# this shows the total n by group condition. 
# Note that number of obs differs by item
data |> 
  dplyr::select(electionvig, trust1, dplyr::contains("distrust_"), safety, recruitpref) |>
  dplyr::group_by(electionvig) |> 
  dplyr::summarise(across(.cols = dplyr::everything(), ~sum(is.na(.))))



# very fast and easy way to convert labelled variables in data frame to factor
# (i.e., "factorize") while keeping attributes like variable label and value
# labels. Also able to prefix value label with associated values (`prefix = T`)
# and keep value labels (`keep.labels = T`)
data |> 
  sjlabelled::as_label(prefix = TRUE, keep.labels = TRUE) |>
  dplyr::select(q1:immigrant_crime) |> 
  str()

# this generates a list of tibbles with value counts
data |> 
  dplyr::select(A, B) |> 
  purrr::map(~dplyr::count(data.frame(x = .x), x))

# alternatively
data |> 
  dplyr::select(A, B) |>
  purrr::map(\(x) janitor::tabyl(x, show_na = F) |> 
               janitor::adorn_totals(where = c("row")) |> 
               janitor::adorn_rounding(digits = 2)
             )


# another way to convert to factor ----------------------------------------

lvls <- c(NULL = "NA; no Pre IW; no Post IW",
          NULL = "NA; no Pre IW; no Post IW; form B (1986)",
          agree = "Agree",
          disagree = "Disagree",
          neither = "Neither agree nor disagree (1988 and later only)",
          NULL = "DK; not sure; it depends; can't say; refused to say",
          NULL = "DK; depends; not sure; can't say; refused to say")

cdf2 |>
  dplyr::mutate(dplyr::across(
    c(nosay, nocare),
    ~ sjlabelled::as_label(., keep.labels = T),
    .names = "{col}.f"
  )) |>
  dplyr::mutate(dplyr::across(
    c(nosay.f, nocare.f),
    ~ forcats::fct_relabel(., ~ stringr::str_remove(., "[0-9]+[\\.]\\s*"))
  )) |>
  dplyr::mutate(
    nosay.f = forcats::fct_recode(nosay.f, !!!lvls),
    nocare.f = forcats::fct_recode(nocare.f, !!!lvls)
  )




# a wrapper for `haven::labelled()` ---------------------------------------


data |>
  labelled::labelled(
    x = mil,
    labels = c("Active duty" = 1, "Veteran" = 2, "No military service" = 3),
    label = "Military Experience"
      )


# easy way to edit value or variable labels -------------------------------

# this gets rid of the the annoying prefix embedded in the value labels of ANES
# variables, e.g., "1. Response"
# If no `.col` are specified, then applies to the whole data set
anes2020 |>
  labelled::update_value_labels_with(
    .fn = ~stringr::str_remove(., "^-*[0-9]+[\\.]\\s*")
    )

# method to convert to factor while removing annoying strings from levels in one
# swoop
anes2020 |>
  dplyr::mutate(dplyr::across(
    c(intv_mode, sex, race, partyid, educ, educ5, income),
    ~ forcats::fct_relabel(
      sjlabelled::as_label(.),
      ~ stringr::str_remove(., "^-*[0-9]+[\\.]\\s*")
    ),
    .names = "{col}.fct"
  ))

# get variable labels from dataframe without relying on labelled ----------


# great way to get variable labels from dataframe without relying on labelled
# purrr: 2 ways
purrr::map(data, purrr::attr_getter("label")) |> unlist(use.names = T)
purrr::map(data, attr, "label") |> purrr::list_simplify()
# equivalent to `unlist = FALSE` 
labelled::var_label(data) |> unlist()



# simple way to copy labels over from old data to new data ----------------

sjlabelled::copy_labels(
  df_new = data, 
  df_origin = raw,
  vets_vig_manip, 
  nurse_vig_manip, 
  fire_vig_manip, 
  control_vig_manip
  ) |>  
  dplyr::select(dplyr::contains("_vig_manip")) |> 
  varlab.df(unlist = T)

# sample demographics
data |> 
  dplyr::select(
    age,
    gender,
    dplyr::contains("race"),
    -race_6_text,
    hisp,
    educ,
    mil1, 
    mil2, 
    mil3, 
    partyid_3cat,
    party_strength,
    ideo,
    ideo_lean,
    voted,
    votechoice,
    votepref,
    voteintent
    ) |> 
  sjmisc::frq(show.na = "auto")

datawizard::data_codebook(
  data = data,
  select = c(
    age,
    gender,
    contains("race"),
    hisp,
    educ,
    mil1, 
    mil2, 
    mil3, 
    partyid_3cat,
    party_strength,
    ideo,
    ideo_lean,
    voted,
    votechoice,
    votepref,
    voteintent), 
  exclude = c(race_6_text)
    ) |> 
  datawizard::print_html()

# median age
data |> dplyr::select(age) |> 
  datawizard::describe_distribution(centrality = "median")




## TODO: figure out how to derive city, county, and state from `lucid_zip` var
## TODO: figure out how to derive city, county, state, etc, from Qualtrics `LocationLatitutde` and `LocationLongitude` vars


# handy variable processing stuff ::::::::::::::::::::::::::::::::::::::::::####

# make dummy variables for each category of partyID strength
data |>
  fastDummies::dummy_cols(
    select_columns = "party_strength", 
    ignore_na = T
    ) |> 
  # dplyr::select(dplyr::contains("party_"))
  # dplyr::rename_with(
  #   .cols = contains("party_strength_"),
  #   .fn = janitor::make_clean_names
  #   ) |> 
  # dplyr::rename_with(.fn = \(x)sub("party_strength_", "", x)) |>  
  dplyr::rename(
    stng_dem = party_strength_1,
    weak_dem = party_strength_2,
    lean_dem = party_strength_3,
    indep      = party_strength_4,
    lean_rep   = party_strength_5,
    weak_rep   = party_strength_6,
    stng_rep   = party_strength_7,
  ) |> 
  labelled::set_variable_labels(
    stng_rep = "Strong Republican",
    weak_rep = "Weak Republican",
    lean_rep = "Independent, leans Republican",
    indep      = "True independent",
    lean_dem   = "Independent, leans Democrat",
    weak_dem   = "Weak Democrat",
    stng_dem   = "Strong Democrat"
  ) |>
  dplyr::relocate(
    dplyr::starts_with("stng"),
    dplyr::starts_with("weak"),
    dplyr::starts_with("lean"),
    indep,
    .after = party_strength
  ) |> 
    dplyr::select(
      dplyr::contains("party_"),
      dplyr::starts_with("stng"),
      dplyr::starts_with("weak"),
      dplyr::starts_with("lean"),
      indep
      )

# create an age group variable (groups align with CPS top coding)
data <- data |>    
  mutate(age_cat = 
           forcats::fct_collapse(
             as_factor(age),
             "18-24" = c(18:24),
             "25-34" = c(25:34),
             "35-44" = c(35:44),
             "45-54" = c(45:54),
             "55-64" = c(55:64),
             "65-74" = c(65:74),
             "75-84" = c(75:84),
             "85-94" = c(85:94),
             "95-110"= c(95:110)
             ),
         .after = gender) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into eight groups"
  )

# create another age group variable with fewer age categories
data |>    
  mutate(age_4cat = 
           forcats::fct_collapse(
             as_factor(age),
             "18-34" = c(18:34),
             "35-54" = c(35:54),
             "55-74" = c(55:74),
             "75-85+" = c(75:110)
             ),
         .after = age_cat) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into groups"
  )

# create gender_3cat
data |> 
  dplyr::mutate(gender_3cat = forcats::fct_collapse(
    gender,
    "Male" = "Male",
    "Female" = "Female",
    "Other/Refused" = c("Non-binary / third gender", "Prefer not to say")))

# create education var with fewer categories
data |>
  dplyr::mutate(
    educ_4cat = forcats::fct_collapse(
      educ,
      "H.S. or less" = c(
        "Less than high school degree",
        "High school graduate (high school diploma or equivalent including GED)"
      ),
      "Some college no degree" = "Some college but no degree",
      "College degree" = c(
        "Associate degree in college (2-year)",
        "Bachelor's degree in college (4-year)"
      ),
      "Postgraduate degree" = c(
        "Master's degree",
        "Doctoral degree",
        "Professional degree (JD, MD)"
      )
    ),
    .after = educ
  )

# collapse voted variable 
data |> 
  dplyr::mutate(
    voted.clps = forcats::fct_collapse(
      voted,
      "Voted" = "Yes, I'm sure I voted",
      "Didn't vote" = c("I'm sure I didn't vote", "I don't think I voted"),
      "Unsure/Ineligible" = c("I think I voted","I was not eligible to vote")
      ))

# collapse ideology variable into 3 categories
data |>
  dplyr::mutate(
    ideo_3cat = dplyr::case_when(
      ideo == "Extremely Liberal" | # or
        ideo == "Liberal" | # or
        ideo == "Slightly liberal" ~ "Liberal",
      ideo == "Extremely conservative" | # or
        ideo == "Conservative" | # or
        ideo == "Slightly conservative" ~ "Conservative",
      TRUE ~ ideo # else
      ))

# set variable labels
data |> 
  labelled::set_variable_labels(
    voted.clps = "Turnout 2020",
    ideo_3cat = "Ideology, 4 categories"
  )

# race
# NOTE: for the race item, two respondents wrote "Hispanic" in the text box under "Other" 
data |> 
  dplyr::select(dplyr::contains("race_")) |>
  dplyr::filter(race_6_text != "")


# race, multiple selection item in Qualtrics
# one method I snatched from an old stackoverflow answer
# https://stackoverflow.com/a/61362245
# this results in the same above, except it inevitably converts `race` to a
# character the selected race options are simply collapsed into a single string
data |> 
  dplyr::select(rowid, dplyr::starts_with("r.")) |>
  labelled::remove_val_labels() |> 
  tidyr::pivot_longer(r.white:r.other, values_drop_na = T) |> 
  # dplyr::group_by(rowid, value) |> 
  dplyr::summarise(
    race = paste(name, collapse = " & "), 
    .by = c(rowid, value)
    ) |> 
  dplyr::filter(value==1) |> 
  dplyr::select(-value) |> 
  dplyr::distinct(race)

# NOTE: this is a freq table on how many races were selected in the
# multi-response question
data |> 
  dplyr::select(dplyr::starts_with("r.")) |> 
  dplyr::rowwise() |>
  dplyr::mutate(
    race = sum(dplyr::across(dplyr::starts_with("r.")), na.rm = T),
    race = dplyr::na_if(x = race, 0)
                ) |>
  labelled::add_value_labels(
    race = c("one race" = 1, "two races" = 2, "three races" = 3), 
    .strict = T
  ) |> 
  sjmisc::frq(race)


# annoying way to find out that there are 6 missing values for the race
# multiple-choice question
data |> 
  dplyr::select(dplyr::starts_with("r.")) |>
  dplyr::filter(is.na(r.white), is.na(r.black), is.na(r.aman),
                is.na(r.asian), is.na(r.nhpi), is.na(r.other))


## Useful and Good to know but unnecessary :::::::::::::::::::::::::::::::::####

# convert survey condition vars to labelled numeric vars `<dbl+lbl>`

# NOTE: because these are character vectors `<chr>` they must first be converted
# to numeric (double or integer) vectors. However, I can just use `as.numeric()`
# for these character vectors because `NA` values will be introduced by
# coercion. Instead, I use `dplyr::case_when()`, which is a little like
# `dplyr::if_else` (dplyr version of base R `ifelse()`) but easier to use. 
# I would need multiple, chained, `dplyr::if_else()` statements to convert each
# character vector to numeric without introducing `NA` by coercion.

# `dplyr::case_when()` tests a vector against multiple conditions
# `dplyr::if_else()` tests a vector against a single condition
# base R `ifelse()` is slower and has other issues 
# see (https://r4ds.hadley.nz/logicals.html#fn4)


# convert character `<chr>` vars to numeric `<dbl>`
data <- data |>  
  dplyr::mutate(
    electionvig = dplyr::case_when(
      electionvig == "control" ~ 0,
      electionvig == "vets" ~ 1,
      electionvig == "nurse" ~ 2,
      electionvig == "firefighter" ~ 3,
      TRUE ~ NA
    )
  ) |> 
  dplyr::mutate(
    disability = dplyr::case_when(
      disability == "jensen" ~ 1,
      disability == "washington" ~ 0,
      TRUE ~ NA
    )) |> 
  dplyr::mutate(
    drone = dplyr::case_when(
      drone == "Control" ~ 0,
      drone == "RInter" ~ 1,
      drone == "RIntra" ~ 2,
      drone == "DIntra" ~ 3,
      drone == "DInter" ~ 4,
      TRUE ~ NA
    )
  ) |> 
  dplyr::select(electionvig, disability, drone) |> 
  
  # now convert to labelled numeric vars `<dbl+lbl>`
  # also add variable label
  dplyr::mutate(
    electionvig = haven::labelled(
      electionvig,
      label = "Experiment condition for vets survey experiment",
      labels = c("control" = 0, "vets" = 1, "nurse" = 2, "firefighter" = 3)
    )) |>
  dplyr::mutate(
    disability = haven::labelled(
      disability,
      label = "Condition for disability scale",
      labels = c("jensen" = 1, "washington" = 0)
      )) |>
  dplyr::mutate(
    drone = haven::labelled(
      drone,
      label = "Experiment condition for drone survey experiment",
      labels = c(
        "Control" = 0,
        "RIntra" = 1,
        "RInter" = 2,
        "DInter" = 3,
        "DIntra" = 4)
      ))

# character to factor for survey conditions ::::::::::::::::::::::::::::::::####
# NOTE: this is an alternative way to handle conversion of the character
# variables to factor. This would still remove any variable labels, which means
# I would still need to set a variable label after the conversion, or even after
# I "factorized" the full dataset.
  
# convert character class vars to factor vars
# add variable label to each
# assign specific values to each level of each condition (i.e., value labels).
data |>
  dplyr::mutate(
    across(c(electionvig, disability, drone), ~forcats::fct(.))) |>    # <1> 
  
  sjlabelled::var_labels(                                              # <2>
    electionvig = "Experiment condition for vets survey experiment",
    disability = "Condition for disability scale",
    drone = "Experiment condition for drone survey experiment"
    ) |>
  
  sjlabelled::set_labels(                                              # <3>
    electionvig,
    labels = c("control" = 0, "vets" = 1, "nurse" = 2, "firefighter" = 3)
    ) |>
  sjlabelled::set_labels(                                              # <3>
    disability,
    labels = c("jensen" = 1, "washington" = 0)
  ) |> 
  sjlabelled::set_labels(                                              # <3>
    drone,
    labels = c(
      "Control" = 0,
      "RIntra" = 1,
      "RInter" = 2,
      "DInter" = 3, 
      "DIntra" = 4)
  )

# factorize dataset ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####  
# IMPORTANT: Before labelled variables in dataset are converted to factors, save
# the variable labels to a named character vector (each character will be the
# label and associated with the variable column name as attribute "names")

## save data labels
data_labels <- data |> 
  labelled::generate_dictionary() |> 
  labelled::convert_list_columns_to_character() |> 
  dplyr::select(variable, label) |> 
  tibble::deframe()

# NOTE: There are a bunch of different packages or methods to use to factorize
# the dataset. I don't like that I have to break this process down into 3 steps.
# I'd like to find a function or package that allows me to preserve the variable
# labels. I'll need to experiment with this in order to figure out the best
# method. Or I could attempt to make a custom function.

# NOTE on `sjlabelled` options:
# `sjlabelled::as_factor` converts numeric values into a factor with numeric
# levels. `sjlabelled::as_label`, however, converts a vector into a factor and
# uses value labels as factor levels.

## factorize dataset
data |>
  # convert only labelled variables to factors
  # preserve value labels for easier conversion back to numeric
  sjlabelled::as_label(keep.labels = TRUE) |> 
  # re-code any empty/blank levels to NA
  dplyr::mutate(
    dplyr::across(where(is.factor), ~ forcats::fct_recode(., NULL = ""))) |> 
  sjlabelled::get_label() |> 
  tibble::enframe() |> View()

## insert data labels  
data |> 
  labelled::set_variable_labels(!!!data_labels)


# interesting way to create a factor using dplyr::case_when
data |> dplyr::mutate(sex = forcats::fct(
  dplyr::case_when(
    V201600 == 1 ~ "Male", 
    V201600 == 2 ~ "Female", 
    TRUE ~ NA_character_),
  levels = c("Male", "Female")
))



# code from srvyrexploR for ANES 2020 -------------------------------------

# This code mostly follows the R code from the `srvyrexploR` package used in the book "Exploring Complex Survey Data Analysis Using R". The URL where I found their code is:
# https://github.com/tidy-survey-r/srvyrexploR/blob/main/data-raw/anes_2020.R


anes2020 |> 
  dplyr::mutate(
    intv_mode = sjlabelled::as_label(x = V200002, keep.labels = F),
    stratum = sjlabelled::as_label(x = V200010d, keep.labels = F),
    psu = sjlabelled::as_label(x = V200010c, keep.labels = F), # primary sampling unit
    sex = forcats::fct(dplyr::case_when(
      V201600 == 1 ~ "Male",
      V201600 == 2 ~ "Female",
      TRUE ~ NA_character_),
      levels = c("Male", "Female")),
    race = forcats::fct(dplyr::case_when(
      V201549x == 1 ~ "White",
      V201549x == 2 ~ "Black",
      V201549x == 3 ~ "Hispanic",
      V201549x == 4 ~ "Asian NH/PI",
      V201549x == 5 ~ "Native American/ALSK Native",
      V201549x == 6 ~ "Other/multiple race",
      TRUE ~ NA_character_),
      levels = c("White", "Black", "Hispanic", "Asian NH/PI", 
                 "Native American/ALSK Native", "Other/multiple race")),
    partyid = forcats::fct(dplyr::case_when(
      V201231x == 1 ~ "Strong Democrat",
      V201231x == 2 ~ "Not very strong Democrat",
      V201231x == 3 ~ "Independent-Democrat",
      V201231x == 4 ~ "Independent",
      V201231x == 5 ~ "Independent-Republican",
      V201231x == 6 ~ "Not very strong Republican",
      V201231x == 7 ~ "Strong Republican",
      TRUE ~ NA_character_
    )),
    educ = forcats::fct(dplyr::case_when(
      V201510 <= 0 ~ NA_character_,
      V201510 == 1 ~ "Less than HS",
      V201510 == 2 ~ "High school",
      V201510 == 3 ~ "Some College",
      V201510 == 4 | V201510 == 5 ~ "Associate degree",
      V201510 == 6 ~ "Bachelor's degree",
      V201510 == 7 ~ "Master's degree",
      V201510 <= 8 ~ "Doctoral degree/Professional degree",
      TRUE ~ NA_character_
    )),
    educ_5cat = forcats::fct(dplyr::case_when(
        V201510 == 1 ~ "Less than HS",
        V201510 == 2 ~ "High school",
        V201510 <= 5 ~ "Post HS",
        V201510 == 6 ~ "Bachelor's",
        V201510 <= 8 ~ "Graduate",
        TRUE ~ NA_character_
    )),
    income = forcats::fct_relabel(
      sjlabelled::as_label(V201617x), ~stringr::str_remove(.,"[0-9]+[\\.]\\s*")),
    income_7cat = forcats::fct_collapse(
      income,
      "Under $20k" = c("Under $9,999", "$10,000-14,999", "$15,000-19,999"),
      "$20k to < 40k" = c("$20,000-24,999", "$25,000-29,999", "$30,000-34,999", "$35,000-39,999"),
      "$40k to < 60k" = c("$40,000-44,999", "$45,000-49,999", "$50,000-59,999"),
      "$60k to < 80k" = c("$60,000-64,999", "$65,000-69,999", "$70,000-74,999", "$75,000-79,999"),
      "$80k to < 100k" = c("$80,000-89,999", "$90,000-99,999"),
      "$100k to < 125k" = c("$100,000-109,999", "$110,000-124,999"),
      "$125k or more" = c("$125,000-149,999", "$150,000-174,999", "$175,000-249,999", "$250,000 or more")
      )
    ) |> 
  dplyr::mutate(dplyr::across(dplyr::where(is.factor),
      ~forcats::fct_relabel(., ~stringr::str_remove(., "^-*[0-9]+[\\.]\\s*"))))



# getting variable value codes and labels from CPS data -------------------

# one minor issue is that I don't have the value labels for the variables, only the numeric codes
# I hate having to go back to reference a codebook constantly
# I can use `censusapi::listCensusMetadata` to get the values for specific
# variables in the CPS data

censusapi::listCensusMetadata(
  name = "cps/basic/mar", 
  vintage = 2024, 
  type = "values",
  variable_name = "PRCITSHP"
)

cps_state |> 
  dplyr::distinct(PRCITSHP)

# get the value labels for 
censusapi::listCensusMetadata(
  name = "cps/basic/mar", 
  vintage = 2024, 
  type = "values",
  variable_name = "DIVISION"
) |> 
  dplyr::as_tibble()


list_cps_vars <- list("PRCITSHP",
                      "STATE",
                      "DIVISION",
                      "REGION")
purrr::map(list_cps_vars, \(x) censusapi::listCensusMetadata(name = "cps/basic/mar",
                                                             vintage = 2024,
                                                             type = "values",
                                                             variable_name = x))



# replace values/levels with NA -------------------------------------------

# replace factor levels with NA
na_strings <- c("Refused", 
                "Don't know",
                "Inapplicable",
                "Insufficient partial, interview deleted",
                "No post interview",
                "Sufficient partial, breakoff",
                "Error")

anes2024 |>
  naniar::replace_with_na_if(.predicate = is.factor,
                             condition = ~.x %in% na_strings)



# using `naniar` R package, use `replace_with_na_all` to replace any values that
# meet a condition across the entire dataset.
# NOTE: This takes way way too long. It never completed last time I ran it
anes2024 |>
  naniar::replace_with_na_all(condition = ~.x < 0)






# set certain values as NA for selection of variables.

na_vals <- c(
  "Refused"                                 = -9,
  "Don't know"                              = -8,
  "Insufficient partial, interview deleted" = -7,
  "No post interview"                       = -6,
  "Sufficient partial, breakoff"            = -5,
  "Error"                                   = -4,
  "Restricted"                              = -3,
  "Missing"                                 = -2,
  "Inapplicable"                            = -1
)


# tag NA values with `as.tag = TRUE` so that missing values get an information
# tag and value label 
anes2024 |>
  dplyr::mutate(dplyr::across(dplyr::where(labelled::is.labelled),
                ~sjlabelled::set_na(., na = na_vals, as.tag = TRUE)))

# NOTE: setting `as.tag = TRUE` is okay but kind of annoying
# as the tagged NA values are applied to every variable unless specified.



# missing values and related  ---------------------------------------------

# return columns in dataframe that are completely blank/empty
anes_rbind |> dplyr::select(dplyr::where(~all(is.na(.))))


# one way to get the number of missing NA values across columns
anes2024 |> 
  dplyr::summarise(dplyr::across(c(nocare.recode, nosay.recode), ~sum(is.na(.))))

# easier way to get `n_miss` that shows more information
# however doesn't tell me the number of valid responses per item
# for that I need to do the math myself
anes2024 |> 
  dplyr::select(nocare, nosay) |>
  naniar::miss_var_summary(add_cumsum = T) |> 
  dplyr::mutate(n_valid = nrow(anes2024)-n_miss)

# this generates a list of tibbles with value counts
anes2024 |> 
  dplyr::select(nocare, nosay) |>
  purrr::map(\(x) janitor::tabyl(x, show_na = F) |> 
               janitor::adorn_totals(where = c("row")) |> 
               janitor::adorn_rounding(digits = 2)
             )

# alternatively
anes2024 |> 
  dplyr::select(nocare, nosay) |>
  # tidyr::drop_na() |> 
  purrr::map(~dplyr::count(data.frame(x = .x), x)) |> 
  purrr::map(\(x) tidyr::drop_na(x) |> janitor::adorn_totals(where = "row"))




# computing average of index ----------------------------------------------

# alternate ways of computing average of an index variable 
# mean of sum = sum(c(x1, x2))/n
mean(as.numeric(c(anes2024$nocare, anes2024$nosay)), na.rm = T) 
# deconstruct this step-by-step:

# step-1 combine A and B
AB <- c(anes2024$nocare, anes2024$nosay)
# note this is different from c(A + B)
# for c(A + B), values are added together but n stays the same

# step-2 sum of AB.
sumAB <- sum(AB, na.rm = T) # = 22295 
# same: sum(c(anes2024$nocare, anes2024$nosay), na.rm = T)

# step-3 get valid n = sum of obs that are not missing from combined items
valid_n <- sum(!is.na(AB)) # = 9860
# this is the same as; 
sum(!is.na(anes2024$nocare)) + sum(!is.na(anes2024$nosay)) # = 9860

# step-4 get mean of sum
# mean of sum = sum of A and B divided by valid_n
sumAB/valid_n
sum(AB, na.rm = T)/sum(!is.na(AB))
22295/9860


# sum of means = sum(mean(x1), mean(x2))/k
# k = number of items, variables, elements in set. 
sum(mean(anes2024$nocare, na.rm = T), mean(anes2024$nosay, na.rm = T))/2 
(2.150365)+(2.371857)
4.522222/2

# note this result is different from the mean of sum because the mean of item A is derived from a different valid_n than the valid_n of item B. That is, each column has a different number of missing values and as such a different n.
# So the mean of the sum = sum of the means ONLY when the means in the latter are drawn from the same number of elements/observations.


# random ------------------------------------------------------------------

# just a dataframe (tbl) of presidents with the dates in office
pres_terms <- tibble::tribble(
  ~president,   ~serv_start,         ~serv_end,
  "Roosevelt",    "March 4th, 1933",     "April 12th, 1945",
  "Truman",       "April 12th, 1945",    "January 20th, 1953",
  "Eisenhower",   "January 20th, 1953",  "January 20th, 1961",
  "Kennedy",      "January 20th, 1961",  "November 22nd, 1963",
  "Johnson",      "November 22nd, 1963", "January 20th, 1969",
  "Nixon",        "January 20nd, 1969",  "August 9th, 1974",
  "Ford",         "August 9th, 1974",    "January 20th, 1977",
  "Carter",       "January 20th, 1977",  "January 20th, 1981",
  "Reagan",       "January 20th, 1981",  "January 20th, 1989",
  "Bush",         "January 20th, 1989",  "January 20th, 1993",
  "Clinton",      "January 20th, 1993",  "January 20th, 2001",
  "BushJr",       "January 20th, 2001",  "January 20th, 2009",
  "Obama",        "January 20th, 2009",  "January 20th, 2017",
  "Trump",        "January 20th, 2017",  "January 20th, 2021",
  "Biden",        "January 20th, 2017",  "January 20th, 2025") |>
  dplyr::mutate(pres_num = as.character(seq(32, 46, by = 1)), .before = president) |>
  dplyr::mutate(
    across(c(serv_start, serv_end), ~lubridate::mdy(.)),
    start_year = lubridate::year(serv_start),
    end_year = lubridate::year(serv_end),
    term = lubridate::interval(start = serv_start, end = serv_end, tzone = "UTC")) 

pres_terms


# add column that indicates who was president for whatever year
cdf |> 
  dplyr::group_by(year) |> 
  dplyr::mutate(president = dplyr::case_when(
    year == 1948 ~ "Roosevelt",
    year %in% c(1945:1953) ~ "Truman",
    year %in% c(1953:1961) ~ "Eisenhower",
    year %in% c(1961:1963) ~ "Kennedy",
    year %in% c(1963:1969) ~ "Johnson",
    year %in% c(1969:1974) ~ "Nixon",
    year %in% c(1974:1977) ~ "Ford",
    year %in% c(1977:1981) ~ "Carter",
    year %in% c(1981:1989) ~ "Reagan",
    year %in% c(1989:1993) ~ "Bush",
    year %in% c(1993:2001) ~ "Clinton",
    year %in% c(2001:2009) ~ "BushJr",
    year %in% c(2009:2017) ~ "Obama",
    year %in% c(2017:2021) ~ "Trump",
    year %in% c(2021:2025) ~ "Biden",
    TRUE ~ NA
  )) |> 
  dplyr::ungroup()


# for later ---------------------------------------------------------------

# brief review ------------------------------------------------------------



# how many of the panel moved state or region  
anes_panel |>
  dplyr::summarise(
    moved_region = sum(moved_region.dum, na.rm = T),
    moved_st_within_region = sum(moved.dum == 1 & moved_region.dum == 0, na.rm = T),
    n_moved = sum(moved.dum, na.rm = T),
    moved_twice = sum(moved==3 | moved==4, na.rm = T)
    )

# 198 people in the panel moved between the 2016 and 2024 ANES; 176 moved only
# once, 22 moved twice within the same period. 90.43% of the panel did not move
# states across waves; approx 10% (0.096) moved to a different state. 117, or
# approximately 6% (0.057) of the panel moved to a different region between
# waves.


# detailed account of panel members who relocated to different states, and where
anes_panel |>
  dplyr::filter(moved > 0) |> 
  dplyr::group_by(caseid2016) |> 
  dplyr::summarise(
    moved = moved,
    res2016 = st_abb_2016,
    res2020 = st_abb_2020,
    res2024 = st_abb_2024,
    region2016 = census_region_2016,
    region2020 = census_region_2020,
    region2024 = census_region_2024,
    where = dplyr::case_when(
      moved == 1 ~ paste(st_abb_2016,"to", st_abb_2020),
      moved == 2 ~ paste(st_abb_2020,"to", st_abb_2024),
      moved == 3 ~ paste(st_abb_2016, "to", st_abb_2020, "to", st_abb_2024),
      moved == 4 ~ paste(st_abb_2016, "to", st_abb_2020, "back to", st_abb_2024),
      moved == 0 ~paste("remained in", st_abb_2016))
    ) |>
  dplyr::arrange(moved)

# detailed account of panel members who relocated to different regions and where
anes_panel |>
  dplyr::mutate(dplyr::across(dplyr::contains("census_"), ~sjlabelled::as_label(.), .names = "{col}.fct")) |> 
  dplyr::filter(moved_region > 0) |> 
  dplyr::group_by(caseid2016) |> 
  dplyr::summarise(
    moved_region = moved_region,
    where = dplyr::case_when(
      moved_region == 1 ~ paste(census_region_2016.fct, "->", census_region_2020.fct),
      moved_region == 2 ~ paste(census_region_2020.fct, "->", census_region_2024.fct),
      moved_region == 3 ~ paste(census_region_2016.fct, "->", census_region_2020.fct, "->", census_region_2024.fct),
      moved_region == 4 ~ paste(census_region_2016.fct, "->", census_region_2020.fct, "->", census_region_2024.fct))
    ) |>
  dplyr::arrange(moved_region) |> 
  print(n = Inf)

