
# The older NES surveys and documentations are far more difficult to parse through. The order of the survey items for each form (A, B) for each wave (wave 1, wave 2) had to be recorded in the intro codebook. Neither the question number nor the variable code represent the oder in which the questions were actually asked of respondents. So there's a whole section "QUESTION ORDER" that is supposed to clarify the question order, but it doesn't do a great job. For instance, for Wave 2, Form A, the "Regime-based trust and efficacy" items are said to range from question numbers "A3a-A5h" and variable number/code V875169-V875175. However, There is no "A5h" question number. If I match the variable number with the question number, VAR 875169 and A3a, the range of question numbers actually seems to be A3a-A3k. Also, another item should read "VAR 875160" but instead reads "VAR 875169". V875160 refers to a feeling thermometer for poor people (A2b), whereas V875169 refers to the NOSAY external efficacy variable.


# 1987 Pilot Guide to location of variables
# Variable numbers --- Description of data
# 820001-820802 --- 1986 National Election Study
# 872001-872317 ----- 1987 Pilot study, Wave 1: fixed alternative survey variables and field admin variables (from CATI application)
# 873001-874061 ----- 1987 Pilot study, Wave 1: open-end coding of study variables (from DDE application)
# 875001-875330 ----- 1987 Pilot study, Wave 2: fixed-alternative vars (from CATI application)
# 876001-877067 ----- 1987 Pilot study, Wave A: open-end coding of study vars (from DDE application) 

# set up ------------------------------------------------------------------

# set seeed
set.seed(12345)

# load some custom functions
source(here::here('utils', 'funs.R'))

# load packages
# library(tidyverse)
# library(srvyr)

# load data
pilot <- haven::read_dta(here::here("data-raw", "anes", "NESPIL87.dta"))

class(pilot)
pilot



pilot |> 
  dplyr::select(
    V860004, # CASEID
    V860006, # State abbreviation and Congressional district number
    V860024, # Primary Area Name (?)
    V860025, # Census Region
    V860026, # FIPS state and city code
    V860027, # FIPS 80 Standard Metropolitan Statistical Area codes
    V860028, # FIPS 80 Standard Consolidated Statistical Area codes
    V860029, # Population size: place of interview (1980 Census based)
    V860030, # Actual population of location of interview (1980)
    V860036, # Sampling Error code (no idea, "Actual number is coded")
    V860047, # State ICPSR Code
    V860048, # FIPS state code
    V872001, # Cati Case ID (there are gaps in the numbering series)
    V872002, # Control ID
    V872004, # Time zone
    V872005, # DST Indicator (daylight savings time?)
    V872006, # Sample type
    V872007, # Form of questionnaire
    V872008, # Status
    V872009, # Result code (1 = Interview, 5 = partial interview)
    V872035, # Stratum
    V872037, # Sample ID
    V872048, # Sex
    V872049, # Age
    V872094, # IW Start date
    V872095, # IW End date
    V872140, # Release date
    V872141, # Replicate code,
    V872151, # President approval (Reagan)
    V872152, # Pres approval strength
    V872180, # Party ID
    V872181, # Party ID strength (Republican)
    V872182, # Party ID strength (Democrat)
    V872270, # Party ID summary var
    
    V875001, # CATI CASE ID WAVE 2
    V875002, # Control ID
    V875006, # Sample type
    V875007, # Questionnaire form (1 = Form A, 2 = Form B)
    V875008, # Status
    V875009, # Result code (1 = IW, 5 = partial IW)
    V875035, # Stratum
    V875037, # Sample ID
    V875048, # Sex
    V875049, # Age
    V875094, # IW start date (Wave 2)
    V875095, # IW end date (Wave 2)
    
    V875169, # NOSAY
    V875170, # COMPLEX
    V875171, # LEGAL
    V875172, # FEWPOWER
    V875173, # NOCARE
    V875174, # VOTESAY
    V875175, # FINALSAY
    V875216, # QUALIFY
    V875217, # HONEST
    V875218, # TRUSTED
    V875219, # PEOTHINK
    V875220, # PEOVOTES
    V875221, # SERVANTS
    V875222, # LOSETUCH
    V875223, # LOSETUCH FOLLOW-UP
    V875267, # SELFQUAL
    V875268, # UNDRSTND
    V875269, # OTHERS
    V875270, # PUBOFF
    V875271, # NOTSURE
    V875272, # INFORMED
    V875273, # WASTETAX
    V875274, # TRUSTDC
    V875275, # BIGINT
    V875276, # CROOKED
    V875227, # ELECRESP
    V875278, # GOVRESP
    V875315, # BESTGOVT
    V875316, # PROUDGOV
    V875317, # CHNGGOVT
    V875318, # LIVEHERE
    V875320, # TRUSTRGT
    V875321, # FEWBIG
    V875322, # SERVEPUB
    V875323, # TRUTH
    V875324, # CLSEWTCH
    V875325, # PROMISES
    V875330, # MAKELSTN
    V876005 # FORM OF QUESTIONNAIRE (1 = Form A, 2 = Form B)
    )

var_label_tab(pilot$V875169)
