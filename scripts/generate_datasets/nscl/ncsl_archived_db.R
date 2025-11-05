################################################################################
# This script is used to construct a dataset of enacted state legislation from
# 2011 to 2024 from the National Conference of State Legislature's State
# Elections Legislation Archived Database.

# Find the NCSL State Elections Legislation 2011-2024 Archived Database site
# https://www.ncsl.org/elections-and-campaigns/state-elections-legislation-archived-database

# In addition, this script first constructs a codebook/data dictionary
# pertaining to the variables in the NCSL Archived database 2011-2024. I scrape
# the topic categories and descriptions of legislation from the the NCSL State
# Elections Legislation 2011-2024 Archived Database site and then match and
# combine legislation topics and descriptions with the NCSL database derived
# from Joseph Loffredo's GitHub repository.

# At the time of writing this script, Loffredo is a PhD candidate working at
# MIT's Election Data and Science Lab.
# See his [site](https://joseph-loffredo.com/)

# NCSL db is forked from this github repo
# https://github.com/jloffredo2/state-elect-law-db
# Author: Joseph Loffredo
# Institution: MIT
################################################################################

# set seed for reproducibility
set.seed(12345)

# load packages (those commented out may or may not be necessary)
library(tidyverse)
# library(rvest) # Easily Harvest (scrape) web pages

# load some custom functions
'%nin%' <- function(x, table) is.na(match(x, table, nomatch=NA_integer_))

# fork the GitHub repository to local directory
# easiest way to  fork and clone a github repo
# usethis::create_from_github(
#   repo_spec = "https://github.com/jloffredo2/state-elect-law-db.git",
#   destdir = "~/Documents/R/forked_repositories", 
#   fork = T
# )

# path where data is in local directory
ncsl_db_path <- "~/R/forked_repositories/state-elect-law-db/output/ncsl_bill_database_2011_2024.Rdata"

# load NCSL data
load(file = ncsl_db_path)

# rename and format as tibble
ncsl <- ncsl_bill_database |> dplyr::as_tibble() 

# remove from memory
rm(ncsl_bill_database, ncsl_db_path)


# obtain topic category descriptions from HTML webpage :::::::::::::::::::::####

# scrape webpage using `rvest` R package
URL <- "https://www.ncsl.org/elections-and-campaigns/state-elections-legislation-archived-database"
ncsl_topic_desc <- rvest::read_html(URL)

# these are all the topics and descriptions of state legislation from 2011-2024
# derived from the HTML webpage of the URL
topics <- ncsl_topic_desc |> rvest::html_elements("h4") |> rvest::html_text2()
topics <- stringr::str_trim(topics[1:89]) # trim white space from ends of string

descs <- ncsl_topic_desc |> rvest::html_elements("p") |> rvest::html_text2()
descs <- stringr::str_trim(descs[5:93])

# wrap topics and descriptions into a tibble data frame
law_topics <- tibble::tibble(
  topic = topics,
  description = descs
)

# sort/arrange by topic (alphabetical) and add `rowid` column
law_topics <- law_topics |> 
  dplyr::arrange(topic) |> tibble::rowid_to_column()

# add variable names from NCSL db github repo ::::::::::::::::::::::::::::::#### 

# the GitHub repository of the NCSL data base uses custom variable names for
# each legislation topic category. Thing is, these custom names are associated
# with short descriptions of the legislation categories that don't always
# identically match the topics from the webpage. Also, a good number of these
# variable names are not found within the the 2011-2024 NCSL archived database.
# What I do here is create a named-character vector of the variable names with
# the short topic descriptions in order to match these with the topics, topic
# codes, and descriptions I scraped from the HTML page.

# create character vector
# NOTE: some of the topic descriptions are modified to match those scraped from
# the NCSL webpage
mit <- c(
  AUDITS = 	"Audits",
  AVAPPL = 	"Absentee Voting-Application for",
  AVBDIS = 	"Absentee Voting-Distributing Ballots",
  AVBRET = 	"Absentee Voting-Returning Ballots",
  # AVDLIN =  "Absentee Voting-Deadlines",              # not in NCSL db
  AVELIG = 	"Absentee Voting-Eligibility",
  AVEVIP = 	"Early Voting/In-Person Absentee",
  AVMIOV = 	"Absentee Voting-Military/Overseas",
  AVMISC = 	"Absentee Voting-Misc.",
  AVMOVE = 	"Absentee Voting-MOVE Act",
  AVNOEX = 	"Absentee Voting-No Excuse",
  AVPERM = 	"Absentee Voting-Permanent Status",
  BACAND = 	"Ballot Access-Candidates",
  # BACURE =  "Ballot Return, Verification, and Cure", # not in NCSL db
  BALDES = 	"Ballots-Required Number, Format & Design",
  # BAMEAS =  "Ballot measures",                       # not in NCSL db
  BAPART = 	"Ballot Access-Parties",
  CANQUL = 	"Candidates-Qualifications and Running for Office",
  CANRTR = 	"Candidates-Resign to Run",
  CANWDW = 	"Candidates-Withdrawal/Death",
  CANWRI = 	"Candidates-Write-in",
  CNTEST = 	"Election Contests (Court Challenges)",
  CRIMES = 	"Election Crimes",
  CYBSEC = 	"Cybersecurity",
  DATART = 	"Election Data-Collection/Retention of",
  # DGVOTE =  "Digital/electronic voting",            # not in NCSL db
  DUALFU = 	"Fusion/Dual-Party",
  ECONPV = 	"Electoral College-National Popular Vote",
  EDHOLI = 	"Election Day Holiday",
  # ELAUTH =  "Shifts in Election Authority",         # not in NCSL db
  ELCOST = 	"Costs of, and Funding for, Elections",
  ELDATE = 	"Dates of Elections",
  ELECOL = 	"Electoral College",
  ELEING = 	"Electioneering",
  EMEDIS = 	"Emergencies/Disasters",
  EOCAMP = 	"Election Officials-Campaign Activities",
  # EOGENR =  "Election Official-General (anything broadly related to election official)", # see note
  EOLOCA = 	"Election Officials-Local",
  EOSTWD = 	"Election Officials-Statewide",
  # EPLOCL =  "Local election procedures",           # not in NCSL db
  EXPOLL = 	"Exit Polling",
  FILING = 	"Offices-Method of Filling",
  INVOTE = 	"Internet/Electronic Voting",
  # LNCACC =  "Language accommodations",            # not in NCSL db
  MAILVO = 	"Mail Voting",
  MISCEL = 	"Miscellaneous",
  POLPAR = 	"Political Parties",
  POLWAT = 	"Poll Watchers, Poll Challengers, Election Observers",
  PPACES = 	"Polling Places-Disabled Access",
  # PPGENR =  "Polling Places-General (nonspecific)", # general category, see note
  PPLOCA = 	"Polling Places-Locations",
  PPPROC = 	"Polling Places-Arrangement of/Procedures at",
  PPVCEN = 	"Polling Places-Vote Centers",
  PPVHRS = 	"Polling Places-Hours",
  PREDEF = 	"Precincts",
  PRIDAT = 	"Primaries-Dates",
  PRIMIS = 	"Primaries-Misc.",
  PRIPUS = 	"Primaries-Presidential",
  PRIRNF = 	"Primaries-Runoff",
  PRITYP = 	"Primaries-Types",
  PROVOT = 	"Provisional Ballots",
  PTDRES = 	"DREs",
  PWCOMP = 	"Poll Workers-Compensation",
  PWMISC = 	"Poll Workers-Misc.",
  PWQUAL = 	"Poll Workers-Selection/Qualifications of",
  PWTRAI = 	"Poll Workers-Training",
  PWYOTH = 	"Poll Workers-Youth",
  # RECALL =  "Recall elections",                 # not in NCSL db
  RECOUN = 	"Recounts",
  # REDIST =  "Redistricting",                    # not in NCSL db
  # REGAGY =  "Registration-Agencies",            # not in NCSL db
  REGAPP = 	"Registration-Application Form/Content",
  REGATO = 	"Registration-Automatic",
  REGCVL = 	"Registration-Statewide Voter Registration Databases",
  REGDRI = 	"Registration Drives",
  REGDTE = 	"Registration-Deadline",
  REGEDY = 	"Registration-Election Day or Same Day",
  # REGELE = 	"Registration-Electronic",
  # REGGEN =  "Registration-General", # general category, see note 
  REGIDR = 	"Registration-Eligibility ID Required",
  REGLST = 	"Registration-List Maintenance",
  REGMSC = 	"Registration-Misc.",
  REGONL = 	"Registration-Online",
  REGPRE = 	"Registration-Preregistration for 16- and 17-year-olds",
  REGSDL = 	"Registration-Sale/Distribution/Use of Lists",
  REPRES = 	"Election Results/Canvass, Reporting of",
  RUNOFF = 	"Runoff Elections",
  SPELEC = 	"Special Elections",
  STVOTE = 	"Straight Ticket Voting",
  TECHSS = 	"Voting Equipment/Technology-Selection & Standards",
  TFSCIC = 	"Task Forces/Study Commissions/Interim Committees",
  VACNCY = 	"Vacancies",
  VCOUNT = 	"Counting Votes",
  VEDINF = 	"Voter Education/Information",
  VOTAFW = 	"Voters-Absence from Work",
  VOTAGE = 	"Voters-Age",
  VOTAST = 	"Voters-Assistance to",
  VOTEME = 	"Alt Voting Methods (Ranked Choice, etc.)",
  VOTFVR = 	"Voters-Incarceration and Voting",
  VOTMQU = 	"Voters-Miscellaneous Qualifications",
  VOTRID = 	"Voter Identification",
  # VOTSEC =  "Voting security (polling places, drop box, counting)", # not in NCSL db
  VSSCST = 	"Voting System Testing/Security/Storage",
  # VTDROP =  "Ballot Drop-off Locations",                # not in NCSL db
  VTRCHA = 	"Challenges to Voters"
)

# From reviewing the source code from the MIT github repo, there were three general
# topic categories created from multiple legislation categories.
# EOGENR = max(EOCAMP,EOLOCA,EOSTWD)
# PPGENR = max(PPPROC,PPACES,PPVHRS,PPVCEN)
# REGGEN = max(REGAPP,REGATO,REGDRI,REGDTE,REGEDY,REGELE,REGIDR,REGMSC,REGPRE)

# These three general topics are in the NCSL db but are unique to the MIT GitHub
# repo. Which means these general topics won't match to the ones in the
# `law_topics` data frame. I excluded these from the character vector to match
# the MIT variable names with the topics and descriptions in the`law_topics`
# data frame.

# URL to source code
# (https://github.com/jloffredo2/state-elect-law-db/blob/main/code/ncsl.R)


# enframe the named-character vector into a two-column tbl-df
mit <- tibble::enframe(mit)

# rename column names
mit <- mit |> dplyr::rename(topic = value, var = name) 

# sort/arrange by topic (alphabetical) and add `rowid` column
mit <- mit |> dplyr::arrange(topic) |> tibble::rowid_to_column()

# now that all topic descriptions and var names are of equal length and match, I
# can add the var names as a column to the `law_topics` df
ncsl_topic_dict <- mit |>
  dplyr::left_join(law_topics) |>
  # arrange in alphabetical order of variable column names in ncsl db
  dplyr::arrange(var) |>
  # remove old `rowid` (now out of order)
  dplyr::select(-rowid) |> 
  # replace with new `rowid` column
  tibble::rowid_to_column()

# nice markdown table
# ncsl_topic_dict |> 
#   pander::pandoc.table(style = "grid", justify = "left", split.table = Inf)

# save the topic descriptions and variable names :::::::::::::::::::::::::::####

# save as .csv
write.csv(
  x = ncsl_topic_dict, 
  file = "~/R/My_projects/exteff-ts/resources/ncsl_topic_descriptions_dictionary.csv")

# Add variable labels to NCSL archived db ::::::::::::::::::::::::::::::::::####

# deframe var names and the topic descriptions into a named character vector
topic_labs <- ncsl_topic_dict |>  
  dplyr::mutate(topic_desc = stringr::str_glue("{topic}: {description}")) |>
  dplyr::select(var, topic_desc) |> 
  tibble::deframe()

# add topic descriptions as variable labels to variable columns that match the
# names of the character vector
ncsl <- ncsl |>  
  labelled::set_variable_labels(!!!topic_labs)

# add variable labels to other columns in the data set
var_labs <- c(
  UUID           = "Unique identifier in the form `STATE`-`YEAR`-`BILLNUM`",
  YEAR           = "Year tracking organization has tagged bill",
  STATE          = "State bill is introduced in",
  BILLNUM        = "Bill number",
  AUTHORNAME     = "Last name of bill author",
  AUTHORPARTY    = "Party affiliation of bill author",
  BILLSTATUS     = "Current bill status",
  PREFILEDATE    = "Date legislation was prefiled prior to regular session",
  INTRODUCEDDATE = "Date bill is officially introduced",
  LASTACTIONDATE = "Date of bill's last action",
  NCOAUTHORS     = "Total number of coauthors",
  NDEMCOAUTHORS  = "Number of Democratic coauthors",
  NREPCOAUTHORS  = "Number of Republican coauthors",
  COAUTHORS 	   = "JSON string listing all coauthors",
  HISTORY 	     = "JSON string of bill's history log",
  BILLTEXTURL 	 = "URL to bill text",
  REGELE         = "Registration-Electronic",
  EOGENR         = "Election Official-General (anything broadly related to election official). EOGENR = max(EOCAMP,EOLOCA,EOSTWD)",
  PPGENR         = "Polling Places-General (nonspecific). PPGENR = max(PPPROC,PPACES,PPVHRS,PPVCEN)",
  REGGEN         = "Registration-General. REGGEN = max(REGAPP,REGATO,REGDRI,REGDTE,REGEDY,REGELE,REGIDR,REGMSC,REGPRE)" 
)

# add variable labels to NCSL archived db
ncsl <- ncsl |> 
  labelled::set_variable_labels(!!!var_labs)

# create a codebook/data dictionary of NCSL archived database 2011-2024 
ncsl_data_dict <- ncsl |> 
  surveytoolbox::data_dict() |> 
  dplyr::as_tibble() |> 
  dplyr::mutate(col_type = unlist(lapply(ncsl, vctrs::vec_ptype_abbr)))


# NOTE: use `rjson::fromJSON()` to transform COAUTHORS, HISTORY, or BILLTEXTURL
# values into an R vector.
# example
# rjson::fromJSON(ncsl$HISTORY[1])

# save ncsl codebook :::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# save as .csv
write.csv(ncsl_data_dict, file = "data/ncsl/ncsl_archived_db_2011_2024_dict.csv")

# save as .rds
readr::write_rds(ncsl_data_dict, file = "data/ncsl/ncsl_archived_db_2011_2024_dict.rds")

# NCSL State Legislation Database ::::::::::::::::::::::::::::::::::::::::::####
# This section subsets the NCSL db to only include enacted legislation

# Quick Note from ncsl.org userguide (https://www.ncsl.org/resources/ncsl-databases-user-guide)

# Enacted Legislation Databases: Enacted databases contain only resolutions that have been approved by the legislature and bills that have been approved by the legislature and signed by the governor. Enactments are added only once final action has been taken to approve the measure. Legislation is added as identified by NCSL staff and the date of the last update is indicated under the database title. Status filters for this database include “enacted,” which pulls legislation that has been approved by both chambers and signed by the governor and “adopted,” which includes joint and single chamber resolutions that have been approved.

# clean up variable column names
ncsl <- ncsl |> janitor::clean_names()

# This database contains state legislation related to the administration of
# elections introduced in 2011 through 2024.
range(ncsl$year)

# subset to include only legislation enacted into law
stlaws <- ncsl |> 
  # include only bills enacted into law
  dplyr::filter(billstatus == "Enacted") |>
  # add in full state name and state FIPS code
  dplyr::mutate(st_fips = fipio::as_fips(state), 
                st = fipio::fips_state(st_fips),
                .before = state) |> 
  dplyr::rename(st_abb = state) |> 
  labelled::set_variable_labels(
    st = "State where bill was introduced",
    st_abb = "Abbreviated state name",
    st_fips = "State Federal Information Processing Standard (FIPS) code"
  )

# save data set as .rds ::::::::::::::::::::::::::::::::::::::::::::::::::::#### 

readr::write_rds(stlaws, file = "data/ncsl/ncsl_enacted_state_legislation_2011_2024.rds")


# Clear memory
rm(list = ls())



