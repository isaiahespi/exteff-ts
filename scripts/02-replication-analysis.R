# This script attempts to replicate results found in Chamberlain (2012)


# set up  -----------------------------------------------------------------

# set seed for reproducibility
set.seed(12345)

# load packages
# library(tidyverse)
# library(naniar)
# library(collapse) # Advanced and fast data transformation
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data
# library(survey) # Analysis of Complex survey samples

# load some custom functions
source(here::here('utils', 'funs.R'))

# load data in wide and long formats
d <- readr::read_rds(file = "data/replica data/replica_data_wide.rds")
dlong <- readr::read_rds(file = "data/replica data/replica_data_long.rds")

# load cdf as tbl_svy object (i.e., with weight variable applied)
# cdf_wt <- readr::read_rds(file = "data/anes data/anes_cdf_wts.rds")


# Replication data set ----------------------------------------------------

# the `replica` data set is constrained to the data (presumably) available to
# Chamberlain at the time
replica_long <- dlong |> dplyr::filter(year >= 1952 & year <= 2008)

replica <- d |> dplyr::filter(year >= 1952 & year <= 2008)

# The replica dataframe doesn't have data for years prior to 1952 nor beyond
# 2008
range(replica$year)


# Replicate Lagged Dependent Variable Dynamic Linear Regression Model --------

# dynamic model
# simple dynamic model where exteff.indx (Y) is a function of its value four years
# prior (Y_t-4)
# the 4-year-lagged DV is likely to affect current values of the DV 
# if the absolute value of the coefficient on the lagged dependent variable is 
# < 1, then the effect of prior values diminish over time, i.e., the effect of
# any given value of Y will decay overtime.

# regress external efficacy on external efficacy at t-4
replica |> 
  lm(exteff ~ exteff.T4, data = _) |> 
  summary()

# equivalent alternative
# replica |>
#   lm(exteff ~ dplyr::lag(exteff, n = 4, order_by = year), data = _) |>
#   summary()

# so the coefficient of the lagged DV = 0.727, which is less than one. The effect of Y on later values of itself decays overtime. However, bear in mind there's still an effect; average values of external efficacy in the aggregate four years prior have influenced current values by 0.727 for every unit-increase in external efficacy on average.

# here is the same simple lagged dependent variable regression model using a lag of 2-years instead of 4-years
lm(exteff ~ exteff.T2, data = replica) |> summary()

# The model results in a b1 coefficient = 0.3957 (s.e. = 0.1964) that is statistically significant at 90% confidence (p = 0.5914). The difference between these two models suggests that external efficacy four years prior has a stronger influence than does external efficacy only a couple years prior, if at all. That being said, the index of external efficacy was measured in 1952, 1956, and 1960, then every two years from 1964 to 2004, and since 2004 has been measured every 4 years. This means that a using a two-year lag of external efficacy as the predictor in the lagged DV model is limited to 20 observations (n = 20) and only for the years from 1964 to 2004 which severely undermines results from this lagged dependent variable OLS model. Since the ANES stopped administering surveys every two years after 2004, any comparison of external efficacy between general election years and midterm election years is limited to that short time period. Plotting external efficacy aggregated index values over time from the limited data available, however, does hint at some divergent patterns although not much more can be said.

# Plot aggregate external efficacy index values by election year distinguished
# by type of election

d |> 
  dplyr::filter(year >=1952, !is.na(election_yr)) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = exteff, group = election_yr, color = election_yr))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5))+
  ggplot2::scale_x_continuous(
    limits = c(1950, 2025),
    breaks = seq(1950, 2025, by = 2)
  )+
  ggplot2::labs(
    title = "External Political Efficacy Over Time",
    subtitle = "Years Distinguished by Election Type",
    caption = "Source data: ANES Time Series Cumulative Data File", 
    x = "Year", 
    y = "Index Average")+
  ggplot2::theme_bw()+
  ggplot2::theme(
    legend.position = "bottom", 
    legend.title = ggplot2::element_blank(), 
    axis.text.x = ggplot2::element_text(angle = 90))

# In order to lag external efficacy simply by the last time external efficacy
# was measured, I drop all years where it was not measured and lag the DV by 1.
# This isn't a 1-year lag, but rather a lag of the DV by it's preceding value. 

# The number of observations (i.e., ANES years that included external efficacy
# items) is n = 25 up to 2008. The 1-year lag is then simply the aggregated
# index value computed from the prior ANES survey iteration.

# run liner regression model: regress average external efficacy index on
# immediately preceding values, i.e., lag of itself by its prior value

lm(exteff ~ exteff.L1, data = replica) |> summary()


# After running the lagged dependent variable model explained above, the result is pretty similar; average external efficacy in the aggregate is affected by it's most prior value (b1 = 0.6585, se = 0.16), and the estimated coefficient on the lagged dependent variable is less than 1, indicating that the effect decays over time. (This model disregards whether the DV was measured during a general election year or midterm election year.)

# Replication of Table 1 in Chamberlain (2012) --------------------------

# Essentially, this is an attempt to replicate Table 1 in Chamberlain (2012)
# Except, I exclude Congressional approval as another predictor to examine. I
# didn't gather it from Roper.
# Models 1-4 are models for each individual predictor
# models 5 and 6 are models for sets of predictors (excluding Congressional approval)

# Replication of results from Chamberlain Table 1.
jtools::export_summs(
  lm(exteff ~ exteff.T4, data = replica),
  lm(exteff ~ exteff.T4 + ics.T1, data = replica),
  lm(exteff ~ exteff.T4 + presapp.T1, data = replica),
  lm(exteff ~ exteff.T4 + trustgov.T2, data = replica),
  lm(exteff ~ exteff.T4 + ics.T1 + presapp.T1, data = replica),
  lm(exteff ~ exteff.T4 + ics.T1 + presapp.T1 + trustgov.T2, data = replica),
  digits = 3,
  statistics = c("N" = "nobs", "Adj. R-Squared" = "adj.r.squared"),
  coefs = c(
    "External Efficacy (t-4)" = "exteff.T4",
    "Index of Consumer Sentiments (t-1)" = "ics.T1",
    "Presidential Approval (t-1)" = "presapp.T1",
    "Trust in Government (t-2)" = "trustgov.T2",
    "Constant" = "(Intercept)"
  )
)

# The table I generate almost replicates Table 1 from Chamberlain 2012. The coefficients are not identical, but very close. The number of observations, however, differ for at least one model but not others. Models 1, 2, 3, 5, and 6 have same number of observations, but model 4 has two fewer observations compared to Table 1 in Chamberlain (2012).

# For model 4, the N I have is one less than the N in Chamberlain. This
# shouldn't be the case since the data is exactly the same

# Analysis with 2-year lag ------------------------------------------------

# For some reason, Chamberlain decides to lag the dependent variable by four years but lags the trust in government predictor by two years. Starting in 1964 up until 2004, both external efficacy and trust in government were measured every two years; justification for the different lags for external efficacy and trust in government is not provided by Chamberlain nor is the reasoning obvious. 

# Since external efficacy was measured at an irregular interval, the lag of 4 years is difficult to justify in theory. This 4-year lag ignores external efficacy in periods where there was not a general election even though external efficacy was measured every two years from 1964 up until 2004. The idea is that prior levels of external efficacy influence current level of external efficacy, but a lag of four years skips years where external efficacy and trust in government was measured by ANES surveys during midterm election cycles.

# That being said, however, a two-year lag of external efficacy also isn't appropriate given that the OLS model will omit observations due to missing data. This leaves us with 20 observations (n = 20) of the dependent variable and its two-year lagged counterpart covering only the years from 1966 to 2004.

d |> 
  dplyr::filter(year >= 1952 & year <= 2024) |> 
  dplyr::select(year, dplyr::contains("exteff")) |> 
  dplyr::filter(!is.na(exteff), !is.na(exteff.T2)) |>
  print(n = Inf)


# Conducting the same analysis including a two-year lag on external efficacy fails to produce results that mimic those presented in Chamberlain's analysis. This contradicts assertion made by Chamberlain in a footnote where he claimed, "A two-year lag on external efficacy produces results that mimic those presented" [-@chamberlain2012, 121, n9].

lm(exteff ~ exteff.T2, data = replica) |> summary()

# Using the same replicated data, the table below displays results of a second analysis lagging the dependent variable by it's value two years prior (t-2) instead of by its value four years prior (t-4).

jtools::export_summs(
  lm(exteff ~ exteff.T2, data = replica), 
  lm(exteff ~ exteff.T2 + ics.T1, data = replica), 
  lm(exteff ~ exteff.T2 + presapp.T1, data = replica), 
  lm(exteff ~ exteff.T2 + trustgov.T2, data = replica), 
  lm(exteff ~ exteff.T2 + ics.T1 + presapp.T1, data = replica), 
  lm(exteff ~ exteff.T2 + ics.T1 + presapp.T1 + trustgov.T2, data = replica), 
  digits = 3,
  statistics = c("N" = "nobs", "Adj. R-Squared" = "adj.r.squared"),
  coefs = c(
  "External Efficacy (t-2)" = "exteff.T2",
  "Index of Consumer Sentiments (t-1)" = "ics.T1",
  "Presidential Approval (t-1)" = "presapp.T1",
  "Trust in Government (t-2)" = "trustgov.T2",
   "Constant" = "(Intercept)") 
  )

# In this table, no coefficient of any predictor in any of the models, nor of the lagged dependent variable, obtains statistical significance. 


# Lag DV by prior value -----------------------------------------------

# Since the general idea is that prior values of aggregated external efficacy account for a portion of subsequent values of the same, then it seems reasonable enough to simply regress external efficacy on most recent prior values of itself while recognizing that the preceding value of external efficacy was measured during the previous iteration of the ANES survey.

# In a final analysis using the replica data, I run the same analysis lagging the dependent variable by it's preceding value, i.e., the average index value of the previous ANES survey iteration. Moreover, I lag the trust-in-government index by its prior value as well for the models that include the aggregated index.

jtools::export_summs(
  lm(exteff ~ exteff.L1, data = replica), 
  lm(exteff ~ exteff.L1 + ics.L1, data = replica), 
  lm(exteff ~ exteff.L1 + presapp.L1, data = replica), 
  lm(exteff ~ exteff.L1 + trustgov.L1, data = replica), 
  lm(exteff ~ exteff.L1 + ics.L1 + presapp.L1, data = replica), 
  lm(exteff ~ exteff.L1 + ics.L1 + presapp.L1 + trustgov.L2, data = replica), 
  digits = 3,
  statistics = c("N" = "nobs", "Adj. R-Squared" = "adj.r.squared"),
  coefs = c(
  "External Efficacy (prior value)" = "exteff.L1",
  "Index of Consumer Sentiments (t-1)" = "ics.L1",
  "Presidential Approval (t-1)" = "presapp.L1",
  "Trust in Government (prior value)" = "trustgov.L1",
   "Constant" = "(Intercept)") 
  )

# Here results are similar to Table 1; only the lagged dependent variable significantly influences subsequent values of itself (b1 = 0.658, s.e. = 0.162, p < 0.001).

# Table 1 incorporate data up to 2024 -----------------------------

# exclude years prior to 1952 and beyond 2024
d <- d |> 
  dplyr::filter(year >= 1952, year <= 2024)

# Running the same analysis while incorporating the ANES data since 2008 up to
# 2024. This adds four observations, n = 29

d |> 
  dplyr::select(year, exteff, exteff.T4, exteff.L4, exteff.T1, exteff.L1) |> 
  dplyr::filter(!is.na(exteff)) |>
  print(n = Inf)

# NOTE: the lagged DV `exteff.T4` refers to the value of external efficacy at
# t-4, which is four years prior. This is not the same as `exteff.L4` which is
# the 4th previously observed value of external efficacy measured by ANES
# counting back from the most recent (2024).
# For year 2024, `exteff.T4` = 28.5, four years prior
# For year 2024, `exteff.L4` = 38.4, four ANES survey iterations/waves prior but 16 years ago.

d |> 
  lm(exteff ~ exteff.T4, data = _) |> 
  summary()

# Results are similar with incorporation of years since 2008. In fact, according to the simple lagged DV model, the preceding value of external efficacy bears stronger influence on subsequent values of external efficacy in the aggregate (b1 = 0.8497, s.e. = 0.1181, p < 0.001). Although this isn't accounting for other factors. 

# replicating Table 1 while incorporating recent ANES data up to 2024
jtools::export_summs(
  lm(exteff ~ exteff.T4, data = d), 
  lm(exteff ~ exteff.T4 + ics.T1, data = d), 
  lm(exteff ~ exteff.T4 + presapp.T1, data = d), 
  lm(exteff ~ exteff.T4 + trustgov.T2, data = d), 
  lm(exteff ~ exteff.T4 + ics.T1 + presapp.T1, data = d), 
  lm(exteff ~ exteff.T4 + ics.T1 + presapp.T1 + trustgov.T2, data = d), 
  digits = 3,
  statistics = c("N" = "nobs", "Adj. R-Squared" = "adj.r.squared"),
  coefs = c(
    "External Efficacy (t-4)" = "exteff.T4",
    "Index of Consumer Sentiments (t-1)" = "ics.T1",
    "Presidential Approval (t-1)" = "presapp.T1",
    "Trust in Government (t-2)" = "trustgov.T2",
    "Constant" = "(Intercept)"
    )
  )

# Now presidential approval at t-1 is statistically significant, even when
# including the lagged DV at t-4. However, adding ICS at t-1, or ICS t-1 and
# trust in government at t-2 removes statistical significance from lagged
# presidential approval.


# running the same LDV models except now external efficacy and trust in
# government are lagged by their previously observed values
jtools::export_summs(
  lm(exteff ~ exteff.L1, data = d), 
  lm(exteff ~ exteff.L1 + ics.T1, data = d), 
  lm(exteff ~ exteff.L1 + presapp.T1, data = d), 
  lm(exteff ~ exteff.L1 + trustgov.L1, data = d), 
  lm(exteff ~ exteff.L1 + ics.T1 + presapp.T1, data = d), 
  lm(exteff ~ exteff.L1 + ics.T1 + presapp.T1 + trustgov.L1, data = d), 
  digits = 3,
  statistics = c("N" = "nobs", "Adj. R-Squared" = "adj.r.squared"),
   coefs = c(
  "External Efficacy (prior value)" = "exteff.L1",
  "Index of Consumer Sentiments (t-1)" = "ics.T1",
  "Presidential Approval (t-1)" = "presapp.T1",
  "Trust in Government (prior value)" = "trustgov.L1",
   "Constant" = "(Intercept)") 
  )

# Finally, much like Table 1 in Chamberlain's analysis, no predictors save for the lagged dependent variable attain statistical significance. If we choose to understand external political efficacy as beliefs about the responsiveness of government to citizen demands, these results provide us with no supporting information that the survey items validly measure the construct. That is, the public's attitudes about the consumer economy and presidential approval last year, nor the most recent measures of trust in government, seem to bear any weight upon survey items purported to function as measures of beliefs about the responsiveness of government and institutions to citizen demands or preferences. To put in other words, the two items are not measuring what we think they measure. This was Chamberlain's conclusion implied at the end of his research note. And yet, what we think the items measure is semantically, conceptually, or otherwise sensibly unrelated to a coherent conception of external political efficacy. The conventional understanding of external political efficacy is misconstrued as government responsiveness and attempts at its measurement are malformed. In fact, research published a decade ago  [@esaiasson2015]

# If we choose an unconventional, but cogent, understanding---external political efficacy as an individual's appraisal of the public's capacity to shape conditions of the political environment---then not only are these results uninformative, but the model and method of analysis intended to test the coherence of measurement and meaning is inappropriate. Such results are to be expected given that none of the chosen predictors are reflective of the political environment---especially as macro-level contextual variables---and thus should not be expected to bear any influence on external political efficacy. 


# Checking for Autocorrelation --------------------------------------------

# Errors are autocorrelated if the error at one time is correlated with the
# error in the previous time period.

# In an autoregressive model (AR), the absolute value of rho must be less than
# 1. If not, the errors would grow larger in each time period. In an AR(1)
# model, the error is a function of error only in the previous period. In an
# AR(2) model, the error is a function of the error from two previous periods,
# and so on.

# serial autocorrelation or trending in independent variables can bias estimated
# coefficients in a dynamic model that includes a lagged dependent variable.

# Here, I run a run an ols model at time t, and then check to determine
# whether the residuals are correlated with historical residuals

# The Durbin-Watson test has the null hypothesis that the autocorrelation of the
# disturbances is 0. It is possible to test against the alternative that it is
# greater than, not equal to, or less than 0, respectively. This can be
# specified by the alternative argument

# Under the assumption of normally distributed disturbances, the null
# distribution of the Durbin-Watson statistic is the distribution of a linear
# combination of chi-squared variables.

# run an ols model at time t

lmtest::dwtest(exteff ~ ics + presapp + trustgov, data = d)

# ------------------------------------------------------------------






