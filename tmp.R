# set up ---------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load custom functions
source(here::here("utils", "funs.R"))

# load packages
# library(tidyverse)
# library(fipio)

# load data
cdf <- readr::read_rds(file = "data/anes data/anes_cdf.rds")

# load cdf as tbl_svy object (i.e., with weight variable applied)
cdf_wt <- readr::read_rds(file = "data/anes data/anes_cdf_wts.rds")

# Why does model 4 N differ? ---------------------------------------------

# The number of obs for model 4 differs from Chamberlain by 2 but I'm not sure why. The data of the model is, or should be, exactly the same as the data utilized by Chamberlain. The dependent and independent variables are only from the ANES Cumulative Data File.


cdf_wt |> 
  srvyr::select(year,
                case_id,
                dplyr::contains("nocare"),
                dplyr::contains("nosay"),
                dplyr::contains("exteff"), 
                dplyr::contains("trust")) |> 
  srvyr::group_by(year) |> 
  srvyr::summarize(exteff = srvyr::survey_mean(exteff, na.rm = TRUE, vartype = NULL),
                   trustgov = srvyr::survey_mean(trustgov, na.rm = TRUE, vartype = NULL),
                   trustgov.indx = srvyr::survey_mean(trustgov.indx, na.rm = TRUE, vartype = NULL)) |> 
  print(n = Inf)

cdf_wt |> 
  srvyr::select(year, nocare, nosay, dplyr::contains("exteff"), 
                dplyr::contains("trust")) |>
  srvyr::survey_count(year, nocare, vartype = NULL)

sjlabelled::get_labels(cdf_wt$variables$nocare, values = 'p')


cdf_wt$variables |>
  dplyr::count(nocare)


