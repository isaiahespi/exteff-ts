# make data dictionary for ANES Cumulative Data File

library(tidyverse)


# load data
anesCDF <- rio::import(file = "data-raw/anes_timeseries_cdf_csv_20220916.csv")

# load variable list (this is an .csv file converted by Adobe Acrobat from the
# PDF codebook varlist to an Excel file)
anesCDF_varlist <- rio::import(file = "resources/anes_timeseries_cdf_codebook_varlist.csv") |>
  # fix case of cell text in `variable` column
  dplyr::mutate(variable = dplyr::case_when(
    variable == "VERSION" ~ "Version",
    .default = as.character(variable)
  ))

# add variable labels to `anesCDF` data frame ::::::::::::::::::::::::::::::####

# deframe variables with associated variable labels
anesCDF_varlabels <- anesCDF_varlist |> 
  dplyr::rename(label = variable_label) |> 
  dplyr::select(variable, label) |> 
  tibble::deframe()

# assign the variable lables using the splice operator. Labels are assigned via
# matching against the variable name, so variable order does not matter.
anesCDF <- anesCDF |>
  labelled::set_variable_labels(!!!anesCDF_varlabels)

# clean up global environment a little
rm(anesCDF_varlist, anesCDF_varlabels)

# add value labels to `anesCDF` data frame :::::::::::::::::::::::::::::::::####

# SO the time series cumulative data file in `anesr` package is outdated
# compared to what I pulled more recently in placed into my personal `miguk`
# package.

# However, the `anesr` package has the response options that correspond to each
# value code (e.g., [1] Strongly disagree, 0. No Post-election interview) which
# is super helpful. With those, I can check response options and value codes
# while leaving the variables in the data set as named or labelled numeric
# values. Otherwise, in order to get response options for each, I'll have to
# check the official codebooks.

# What I do here is deframe the value labels (label_val in
# `surveytoolbox::data_dict`) in the same way as before and add them to the
# anesCDF data frame.

# load the Timeseries cumulative dataset file from `anesr` package
data(timeseries_cum, package = "anesr")

# generate dictionary from `labelled` R package
dict_anesr <- timeseries_cum |>
  labelled::generate_dictionary()

# Not all of the variables in the `timeseries_cum` have value_labels, so I filter the ones that are filled with the 'NULL' as a character, and save them to a character vector.
no_label_vals <- dict_anesr |> 
  dplyr::filter(value_labels=="NULL") |> 
  dplyr::as_tibble() |> 
  pull(variable) |> 
  dput()

# Here I deframe and save the actual value labels that are not `NULL`
anesr_dict_value_labels <- dict_anesr |> 
  dplyr::filter(value_labels != "NULL") |> 
  dplyr::as_tibble() |> 
  dplyr::select(variable, value_labels) |> 
  tibble::deframe()

# now I add the `anesr` value labels to my anesCDF data frame
anesCDF <- anesCDF |>
  labelled::set_value_labels(!!!anesr_dict_value_labels) 

# assign data dictionary as tibble dataframe using `surveytoolbox` function
anesCDF_dict <- anesCDF |> surveytoolbox::data_dict()

# Now the `anesCDF_dict` has 4 variable columns
# var: the variable identifer in the data set
# label_var: variable label corresponding to each variable column in the dataset
# label_val: value label that gives labels that correspond to each value
# value: the distinct values that correspond to each variable

# save data dictionary to resources directory
readr::write_csv(x = anesCDF_dict, file = "resources/anesCDF_codebook_dictionary.csv")


# remove items from global environment
rm(
  anesr_dict_labels,
  anesr_dict_value_labels,
  dict_anesr,
  timeseries_cum,
  no_label_vals
)







