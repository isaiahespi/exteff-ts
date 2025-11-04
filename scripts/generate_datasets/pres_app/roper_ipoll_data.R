

# set seed for reproducibility
set.seed(12345)

# load package
# library(tidyverse)
# library(rio)
# library(rvest)


# simple function %nin% or %not_in%
'%nin%' <- function(x, table) is.na(match(x, table, nomatch=NA_integer_))


# import Roper topline data .txt file  ------------------------------------

roper_text <- readr::read_lines("data-raw/roper_data/roper-pres-approval-by-question-only-toplines-asof-20250930.txt")


# extract data from txt ---------------------------------------------------

roper_archive_numbers <- stringr::str_extract_all(
  roper_text, 
  pattern = "Roper\\s+#[0-9]+") |> 
  unlist() |> 
  stringr::str_remove_all(pattern = "Roper\\s+#")

question_ids <- stringr::str_extract_all(
  roper_text, 
  pattern = "Question: \\[[A-Z]+[.][A-Z0-9]+[.][A-Z0-9]+\\]") |> 
  unlist() |> 
  stringr::str_remove_all(pattern = "Question:\\s+\\[") |> 
  stringr::str_remove_all(pattern = "\\]")

citations <- roper_text |> 
  stringr::str_extract_all(
    pattern = "Citation:...+ Web"
    ) |>
  unlist() |> 
  stringr::str_remove_all(pattern = "\\s+Web") |> 
  stringr::str_remove_all(pattern = "Citation:\\s+")


# identify and extract question IDs from citations
citation_question_ids <- stringr::str_extract_all(
  citations, 
  pattern = "Question [0-9]+[.] [A-Z0-9]+[.]+[A-Z0-9]+[.\\-]*[A-Z0-9]*[.]*[A-Z0-9]*") |> 
  unlist() |> 
  # remove the `Question #` and pre-fix whitespace
  stringr::str_remove_all(pattern = "Question\\s+[0-9]+[.]\\s+") |>  
  # remove `.` after each question ID
  stringr::str_remove_all(pattern = "[.]$")

# identify and extract question number from citations
citation_question_num <- stringr::str_extract_all(
  citations, 
  pattern = "Question [0-9]+") |> 
  unlist()


# extract the dates for each entry
date_strings <- roper_text |> 
  stringr::str_extract_all(
    "\\[[0-9]+[/][0-9]+[/][0-9]+ [-] [0-9]+[/][0-9]+[/][0-9]+\\]+$"
    ) |>
  unlist()

# extract start dates
start_dates <- date_strings |> 
  stringr::str_extract_all(
    pattern = "[0-9]+[/][0-9]+[/][0-9]+ [-]") |> 
  unlist() |> 
  stringr::str_remove_all("\\s+[-]")

# extract end dates
end_dates <- date_strings |> 
  stringr::str_extract_all(
    pattern = "[-] [0-9]+[/][0-9]+[/][0-9]+") |> 
  unlist() |> 
  stringr::str_remove_all("[-]\\s+")

# extract sample size
sample_sizes <- stringr::str_extract_all(
  roper_text, 
  pattern = "Sample Size: [0-9]+") |> 
  unlist() |> 
  stringr::str_remove_all(pattern = "Sample Size:\\s+")


# extract sample description (i.e., universe)
sample_descs <- stringr::str_extract_all(
  roper_text, 
  pattern = "Sample: .*") |> 
  unlist() |> 
  stringr::str_remove_all(pattern = "Sample:\\s+")

headlines <- roper_text |> 
  stringr::str_extract_all(
    "^.+\\s+\\[[0-9]+[/][0-9]+[/][0-9]+ [-] [0-9]+[/][0-9]+[/][0-9]+\\]+$"
    ) |>
  unlist() |> 
  stringr::str_remove_all("\\s+\\[[0-9]+[/][0-9]+[/][0-9]+ [-] [0-9]+[/][0-9]+[/][0-9]+\\]+$")


# create tibble from extracted data ---------------------------------------

# create tibble dataframe from strings and values extracted from .txt file
roper_toplines_txt <- tibble::tibble(
  archive_number       = roper_archive_numbers,
  question_id          = citation_question_ids,
  start                = lubridate::mdy(start_dates),
  end                  = lubridate::mdy(end_dates),
  question_number      = citation_question_num,
  sample_size          = as.integer(sample_sizes),
  sample_desc          = sample_descs,
  survey_headline      = headlines,
  citations            = citations[1:2067]
  ) |>
  dplyr::arrange(desc(start))


# remove unneeded objects from from memory
rm(
  citation_question_ids,
  citation_question_num,
  citations,
  date_strings,
  end_dates,
  start_dates,
  headlines,
  question_ids,
  roper_archive_numbers,
  roper_text,
  sample_descs,
  sample_sizes
)



# import Roper iPoll toplines as .csv file --------------------------------

# import a fresh download of the same roper topline data as the text file but as
# a .csv file

# This is the same exact data as what is found in the text file. The only
# difference is that the text file contains citations and roper archive numbers.
# Those I would like to add to the dataframe

roper_toplines_raw <- readr::read_csv("data-raw/roper_data/roper-pres approval by question only-toplines-asof-20250930.csv")

dplyr::glimpse(roper_toplines_raw)


# Replace `*` for NA in Response Pct column -------------------------------

# In the raw topline data, a `*` symbol is used to signify that less than 0.5%
# of the sample chose a particular response. 
roper_toplines_raw |> 
  dplyr::filter(
    stringr::str_detect(RespPct, "\\D+") # detects any string that isn't a digit
         ) |> 
  # filter(RespPct == "*") |> 
  dplyr::select(QuestionID, QuestionNote, RespTxt, RespPct)

# Replace any `*` in `RespPct` with NA
# also make column names easier to deal with
roper_toplines_raw <- roper_toplines_raw |>
  janitor::clean_names() |> 
  dplyr::mutate(resp_pct = dplyr::na_if(resp_pct, "*"))



# rename columns, convert certain columns to proper classes ---------------

# rename columns, convert certain columns to proper classes
roper_toplines_clean <- roper_toplines_raw |>
  # rename start and end dates
  dplyr::rename(start = beg_date, end = end_date) |>
  # convert date columns to proper class
  dplyr::mutate(across(c(start, end, release_date), 
                       ~lubridate::mdy(.)), .keep = "unused") |> 
  # convert percentages and sample size to integer classes 
  # (i.e., no decimal, not floating point numbers)
  dplyr::mutate(across(c(resp_pct, sample_size), 
                       ~as.integer(.)), .keep = "unused") 



# Join .txt data frame with .csv data frame -------------------------------

# So I don't want the columns from one dataframe to replace same-name columns in
# the other; I want the unique columns that I derived from the text file to be
# incorporated into the topline data I pulled as a .csv file. Essentially, I am
# incorporating the Roper archive number for every question reference ID, a
# column that includes the full citation per row, and the headline (presumably)
# used in media publications. The headline might potentially help when searching
# the iPoll database, besides the archive number or question reference ID

# this adds in columns extracted from the .txt file of the same data
# notably, the Roper archive number
roper_toplines_clean <- dplyr::left_join(roper_toplines_clean, roper_toplines_txt)

roper_toplines_clean <- roper_toplines_txt |> 
  # dplyr::filter(question_id %nin% ques_id_dupes) |>
  dplyr::inner_join(
    roper_toplines_clean,
    suffix = c(".txt", ".csv"))


# process -----------------------------------------------------------------


# add column identifying president
roper_toplines_clean <- roper_toplines_clean |> 
  mutate(president = dplyr::case_when(
    stringr::str_detect(question_txt, stringr::regex("Roosevelt", ignore_case = T)) ~ "Roosevelt",
    stringr::str_detect(question_txt, stringr::regex("Truman|TRUMAN", ignore_case = T)) ~ "Truman",
    stringr::str_detect(question_txt, stringr::regex("Eisenhower|EISENHOWER", ignore_case = T)) ~ "Eisenhower",
    stringr::str_detect(question_txt, stringr::regex("Kennedy|KENNEDY", ignore_case = T)) ~ "Kennedy",
    stringr::str_detect(question_txt, stringr::regex("Johnson|JOHNSON", ignore_case = T)) ~ "Johnson",
    stringr::str_detect(question_txt, stringr::regex("Nixon|NIXON", ignore_case = T)) ~ "Nixon",
    stringr::str_detect(question_txt, stringr::regex("Ford|FORD", ignore_case = T)) ~ "Ford",
    stringr::str_detect(question_txt, stringr::regex("Carter", ignore_case = T)) ~ "Carter",
    stringr::str_detect(question_txt, stringr::regex("Reagan", ignore_case = T)) ~ "Reagan",
    stringr::str_detect(question_txt, stringr::regex("George Bush", ignore_case = T)) ~ "Bush",
    stringr::str_detect(question_txt, stringr::regex("Clinton", ignore_case = T)) ~ "Clinton",
    stringr::str_detect(question_txt, stringr::regex("George W. Bush|George W. bush", ignore_case = T)) ~ "BushJr",
    stringr::str_detect(question_txt, stringr::regex("Obama", ignore_case = T)) ~ "Obama",
    stringr::str_detect(question_txt, stringr::regex("Trump", ignore_case = T)) ~ "Trump",
    stringr::str_detect(question_txt, stringr::regex("Biden", ignore_case = T)) ~ "Biden",
  ), 
  .before = start)

# add columns for year and month
roper_toplines_clean <- roper_toplines_clean |> 
  mutate(year = lubridate::year(start),
         month = lubridate::month(start,label = T, abbr = T), 
         .before = start)

# include only National samples of adults
roper_toplines_clean <- roper_toplines_clean |> 
  dplyr::filter(
    stringr::str_detect(
      sample_desc, 
      stringr::regex("National adult+", ignore_case = T)))


# There are 126 distinct question wordings
roper_toplines_clean |>
  dplyr::arrange(year) |> 
  dplyr::distinct(question_txt) |> 
  print(n = Inf)


# remove unnecessary rows -----------------------------------

# There is one question that asked for approval/disapproval of previous
# presidents (G. W. Bush). I don't need this one "USGALLUP.99JU04.R30"
roper_toplines_clean |> 
  dplyr::filter(
    stringr::str_detect(
      question_txt, "From what you have heard, read, or remember about former President"
      )
    ) |> 
  dplyr::distinct(question_id, archive_number, source_doc)

# question_id = USGALLUP.99JU04.R30 
# archive number = 31088396       
# source = Gallup/CNN/USA Today Poll

# There's also a couple of duplicate rows that contain identical data from the
# same survey data set. (I confirmed by searching the archive number on Roper
# iPoll. For some reason, there are three different question reference IDs
# linked to the same survey data. Only the first one is reliable.)

# remove duplicates by question ID
roper_toplines_clean <- roper_toplines_clean |> 
  dplyr::filter(question_id %nin% c("USGALLUP.99JU04.R30",
                                    "USGALLUP.04JUL030.R05", 
                                    "USGALLUP.200425.Q05",
                                    "USGALLUP.96E1004T.Q06",
                                    "USGALLUP.219-2.R1",
                                    "USGALLUP.98AG21.R01",
                                    "USGALLUP.03JAN23.R04",
                                    "USGALLUP.200503.Q01"))


# Questions that pertain to approval/disapproval of the way the president is
# handling some particular issue or domain is not of interest. I only need
# general approval ratings
roper_toplines_clean |> 
  dplyr::arrange(year) |> 
  dplyr::filter(stringr::str_detect(
    question_txt, stringr::regex(
      pattern = ".*as president.*", 
      ignore_case = T), 
    negate = T)
    ) |>
  dplyr::distinct(year, month, archive_number, question_id, question_txt) |> 
  print(n = Inf)

# This returns rows where the question text matches the regular expression pattern
roper_toplines_clean |> 
  dplyr::arrange(year) |>
  dplyr::filter(stringr::str_detect(
    question_txt, stringr::regex(
      pattern = ".*is handling...\\s++|is handling the strike problem+|foreign policy+|here at home+", 
      ignore_case = T))
    ) |> 
  dplyr::distinct(year, month, question_txt)

# omit rows where approval question pertains to some issue, policy, or domain
roper_toplines_clean <- roper_toplines_clean |>
  dplyr::arrange(year) |> 
  dplyr::filter(!stringr::str_detect(
    question_txt, stringr::regex(
      pattern = ".*is handling...\\s++|is handling the strike problem+|foreign policy+|here at home+", 
      ignore_case = T))
    )

# For reference, here are the questions just excluded with the corresponding archive numbers and question reference codes
# "31087243" "USGALLUP.42-260.QKT03"  Do you approve or disapprove or President Roosevelt's policies here at home?                             
# "31087239" "USGALLUP.41-256.QKT05"  Do you approve, or disapprove of President Roosevelt's policies here at home?                            
# "31087235" "USGALLUP.41-252.QT13D"  In general, do you approve, or disapprove of President Roosevelt's foreign policy?                       
# "31087235" "USGALLUP.41-252.QK13D"  In general, do you approve, or disapprove, of the way President Roosevelt is handling foreign policy?    
# "31087232" "USGALLUP.41-249.Q08A "  In general, do you approve, or disapprove, of the way President Roosevelt is handling the strike problem?
# "31087231" "USGALLUP.41-248.QKT13F" In general, do you approve or disapprove of the way Roosevelt is handling... the defense program?        
# "31087231" "USGALLUP.41-248.QKT13D" In general, do you approve or disapprove of the way Roosevelt is handling... foreign policy?             
# "31087231" "USGALLUP.41-248.QKT13E" In general, do you approve or disapprove of the way Roosevelt is handling... labor disputes?             
# "31087224" "USGALLUP.41-241.QKT14D" IN GENERAL, DO YOU APPROVE, OR DISAPPROVE, OF THE WAY ROOSEVELT IS HANDLING... LABOR DISPUTES?           
# "31087224" "USGALLUP.41-241.QKT14C" IN GENERAL, DO YOU APPROVE, OR DISAPPROVE, OF THE WAY ROOSEVELT IS HANDLING... REARMAMENT?

# remove question note text that is irrelevant now
# Note: this just removes text that matches the regex pattern, it doesn't remove
# entire rows.
roper_toplines_clean <- roper_toplines_clean |> 
  dplyr::mutate(question_note = stringr::str_remove_all(
    question_note, stringr::regex("[*]+ = less than [.5]+[\\%]*[.]*| percent*[.]*", ignore_case = T)))

# this returns distinct values for `sub_population`. I do not need data that
# consists of only a subportion of the national adult population.
roper_toplines_clean |> 
  dplyr::distinct(sub_population) |> 
  print(n = Inf)

# exclude any 'sub population', retaining only survey data of the national adult
# population
roper_toplines_clean <- roper_toplines_clean |> 
  dplyr::filter(is.na(sub_population))


# n = 1,884
roper_toplines_clean |> 
  dplyr::distinct(archive_number, start, end, .keep_all = T)


# identify and organize response text options -----------------------------

# there are 93 distinct response options, but I'm only interested in the
# categories approve, disapprove, unsure or inapplicable. I found no way to
# quickly identify each response option and categorize them into those
# respective categories; sometimes the response text of a particular row was
# "Approve" but contained a typo, or a different phrase/word/term was used to
# indicate approval/disapproval (e.g., "yes"). The only method to sort all the
# response text options into a few distinct categories was tedious; I copied all
# response option texts to my clipboard, pasted them, and then manually
# organized them.

# copy all response options to clipboard, then paste and organize
# into respective response categories
# roper_toplines_raw |> 
#   # return only the rows with unique/distinct response texts under `resp_txt` 
#   dplyr::distinct(resp_txt) |> 
#   # similar to `data$column`, return each response option 
#   dplyr::pull(resp_txt) |>
#   # dput() puts all elements of resp_txt into a vector, i.e., c("approve",...)
#   dput() |>
#   # this copies them all to the clipboard. 
#   writeClipboard()



# Approve response text ---------------------------------------------------

approve_resp_txt <- c(
  "Approve",
  "Favor",
  "Yes",
  "Aprpove"
)

approve_strength_resp_txt <- c(
  "Fine, best, very good, etc.",
  "Approve--All right, okay, good, approve",
  "Approve--Fine, but has made a mistake in appointments",
  "Strongly approve",
  "Very strongly approve",
  "Not so strongly approve",
  "Approve very strongly",
  "Approve not so strongly",
  "Approve--strongly",
  "Approve--not so strongly",
  "Approve--don't know strength",
  "Approve strongly",
  "Approve not strongly",
  "Approve moderately",
  "Moderately approve",
  "Mildly approve",
  "Approve, Don't know/Refused strength",
  "Approve--don't know/refused strength",
  "Approve, don't know/refused strength",
  "Approve don't know/Refused how strong",
  "Approve, don't know strength",
  "Approve, no strength given",
  "Approve--moderately",
  "Approve--very strongly",
  "Approve--don't know/refused strength (vol.)",
  "Fine, best, very good, etc.",
  "Not so strongly",
  "Approve  don't know/refused strength",
  "Somewhat approve",
  "Approve don't know/Refused strength",
  "APPROVE!"
)


# Disapprove response text ------------------------------------------------

disapprove_resp_txt <- c(
  "Disapprove",
  "Oppose",
  "@Disapprove"
)

disapprove_strength_resp_txt <- c(
  "Disapprove--Poor, not a good administration",
  "Disapprove--Very poor",
  "Disapprove--Spending too much on non-defense",
  "Disapprove--Poor in organizing for production",
  "Disapprove--Do not approve of handling national affairs--financing, unemployment etc.",
  "Disapprove--Trying to do too much himself, get more help, less responsibility",
  "Strongly disapprove",
  "Mildly disapprove",
  "DISAPPROVE!",
  "Not so strongly disapprove",
  "Very strongly disapprove",
  "Disapprove very strongly",
  "Disapprove not so strongly",
  "Disapprove/No opinion",
  "Disapprove-strongly",
  "Disapprove--not so strongly",
  "Disapprove--don't know strength",
  "Disapprove strongly",
  "Disapprove not strongly",
  "Disapprove moderately",
  "Moderately disapprove",
  "Disapprove, Don't know/Refused strength",
  "Disapprove--don't know/refused strength",
  "Disapprove, don't know/refused strength",
  "Disapprove don't know/Refused how strong",
  "Disapprove, don't know strength",
  "Disapprove, no strength given",
  "Disapprove--strongly",
  "Disapprove--moderately",
  "Disapprove--very strongly",
  "Disapprove--don't know/refused strength (vol.)",
  "Somewhat disapprove",
  "Disapprove don't know/Refused strength"
)


# unsure/no opinion/don't know/refused etc. -------------------------------

unsure_resp_txt <- c(
  "No opinion",
  "Don't know/No opinion",
  "Don't know",
  "No answer",
  "Undecided",
  "NO OPINION, DON'T KNOW",
  "No Opinion (vol.)",
  "Don't know/Refused",
  "Refused",
  "Don't know/No answer",
  "No opiniob",
  "Don’t know"
)



# Inapplicable Response Text ----------------------------------------------

inap_resp_txt <- c(
  "Both (Vol.)",
  "Both (vol.)",
  "BOTH (VOL.)",
  "Both",
  "Fifty-fifty",
  "APPROVE AND DISAPPROVE (VOL.)",
  "Approve & disapprove (vol.)",
  "BOTH APPROVE AND DISAPPROVE (VOL.)",
  "Miscellaneous",
  "Foreign policy okay, domestic not so good",
  "Qualified answer"
)

# pivot wider -------------------------------------------------------------

# pivot the topline data into wide format, putting each response option as its
# own column with its respective response percentage in the rows.
# recode response text to a few discrete categories
roper_toplines_wide <- roper_toplines_clean |>
  dplyr::mutate(
    resp_txt = dplyr::case_when(
      resp_txt %in% approve_resp_txt ~ "approve",
      resp_txt %in% approve_strength_resp_txt ~ "approve_strength",
      resp_txt %in% disapprove_resp_txt ~ "disapprove",
      resp_txt %in% disapprove_strength_resp_txt ~ "disapprove_strength",
      resp_txt %in% unsure_resp_txt ~ "unsure",
      resp_txt %in% inap_resp_txt ~ "inap",
      TRUE ~ NA
    )
  ) |>
  tidyr::pivot_wider(
    names_from = resp_txt,
    names_sort = T,
    values_from = resp_pct,
    values_fn = sum
  )


# Remove duplicates -------------------------------------------------------

# There are at least 16 rows that are duplicates of the same data, based on
# archive_number, start date, end date, and approval ratings data. The question
# reference IDs are distinct between duplicates, but the archive number is
# unique. I only need to keep one of each, so the duplicate data can be removed
roper_toplines_wide |> 
  dplyr::select(-approve_strength, -disapprove_strength) |> 
  dplyr::relocate(approve, disapprove, unsure, inap, .after = end) |> 
  dplyr::filter(!is.na(approve), !is.na(disapprove)) |> 
  janitor::get_dupes(archive_number, start, end, approve, disapprove, unsure, inap) |> 
  print(n = Inf)

# remove unnecessary columns and keep only distinct rows based on archive
# number, start and end dates, and approval ratings
# n = 1,883
roper_toplines_wide <- roper_toplines_wide |> 
  dplyr::select(-approve_strength, -disapprove_strength) |> 
  dplyr::relocate(approve, disapprove, unsure, inap, .after = end) |> 
  dplyr::filter(!is.na(approve), !is.na(disapprove)) |> 
  dplyr::distinct(archive_number, start, end, approve, disapprove, unsure, inap,
                  .keep_all = T)


# reorganize columns. arrange by start date in descending order
roper_toplines_wide  <- roper_toplines_wide  |> 
  dplyr::relocate(start, end, .after = question_id) |>
  dplyr::relocate(question_number, .after = citations) |>
  dplyr::rename(citation = citations) |> 
  dplyr::arrange(desc(start))

# identify duplicate rows by archive number, start date and end date
# n = 20
roper_toplines_wide |> 
  janitor::get_dupes(archive_number, start, end)

# Upon review of the duplicate data on the Roper Center's iPoll site itself, the
# following duplicates can be removed.
# I removed any duplicate data that was used in the Roper Trend data.
# It is not clear why but the trend data differed by a few-too-many percentage
# points.

# remove duplicate rows
roper_toplines_wide <- roper_toplines_wide |>
  dplyr::filter(
    question_id %nin% c(
      "USGALLUP.OC1638.R05A",
      "USGALLUP.032439.R09D",
      "USGALLUP.050739.R15E",
      "USGALLUP.011240.RA13C",
      "USGALLUP.42-266.QKT12C",
      "USGALLUP.102149.R02A",
      "USGALLUP.062755.RK02",
      "USGALLUP.97FEB8.R1"
    )
  )


# Quick note on my trouble parsing the Roper iPoll topline data -----------

# NOTE: It appears that some of these duplicates are only found when the
# Question reference ID is searched, not the roper archive number. Some of the
# duplicates have different question numbers and state that the question/data
# was included in the Roper Trends data for the particular president. Other
# times, the same question was duplicated in the data for seemingly no reason.
# It might be that some duplicate response data was due to the fact that some
# respondents answered the question on different forms (e.g.,  half answered
# form "K", the other, form "T"). However, the stated sample size for both
# refers to the overall size of both groups, leading to misleading topline
# results and my general confusion. The topline data is provided (and presented)
# as a simple percentage of respondents who approve, disapprove, etc., however
# it becomes misleading or at least confusing when the sample size refers to the
# overall size rather than the size of the sample who responded to form K, T, or
# whatever. Especially without response frequencies. For what its worth, most of
# the troublesome duplicates are from prior to 1952, so they'll be cut from the
# final replication analysis anyway. The lesson, I guess, is to rely on the
# Roper archive number rather than the question reference ID, but that isn't a
# full-proof approach as some surveys used different forms.

# For example, there is at least two archive numbers which refer to a data set
# from a survey where respondents answered form "K" or "T". I kept these, but it
# should be noted that the sample size (in the `sample_size` column) refers to
# the overall sample size pertaining to the survey data set corresponding to the
# Roper archive number. Although the topline percentage response data sums to
# 100 (most of the time...), those proportions are only, at best, half of the
# total sample size. It is fine to present the response percentage data seeing
# as how it refers to independent samples; the troubling part is that the actual
# sample size of that independent sample couldn't be determined. The Roper
# Center includes a study note that states "Sample size is approximate".

roper_toplines_wide |> 
  janitor::get_dupes(archive_number)

# Show duplication question IDs in Roper topline data ---------------------


# There are at least 10 duplicate question reference IDs: five question IDs
# for Carter match five question IDs for Obama. The archive numbers are
# distinct, but the citation I derived does not include the roper archive number
# referring to the actual data set (if available).

# return duplicate question IDs, along with archive number, etc.
roper_toplines_wide |> 
  janitor::get_dupes(question_id) |>
  dplyr::select(archive_number, question_id, dupe_count, president, citation)



# Final processing and organizing -----------------------------------------

# reorganize columns
roper_toplines_wide <- roper_toplines_wide |> 
  dplyr::relocate(president, .before = start) |> 
  dplyr::relocate(question_number, survey_headline, citation, .after = study_note)


# final check -------------------------------------------------------------

# no rows of `approve` or `disapprove` should be missing (i.e., NA)
roper_toplines_wide |> dplyr::filter(is.na(approve))
roper_toplines_wide |> dplyr::filter(is.na(disapprove))

# The column variable `unsure` includes any "No opinion", "Don't know",
# "Refused", etc.. The `inap` includes any NA, missing, or responses that were inapplicable
# as either `approve` or `disapprove` (e.g., "Both Approve and Disapprove", "*")
roper_toplines_wide |> 
  dplyr::filter(is.na(unsure)) |> 
  dplyr::select(archive_number, question_id, president, year, 
         approve, disapprove, unsure, inap) |> 
  print(n = Inf)

# The `unsure` column for at least three observations needs to be corrected from
# NA to the actual value in the data set.

# this shows the actual response percent values for the three observations 
roper_toplines_clean |> 
  dplyr::filter(archive_number %in% c("31089821", "31089745", "31088524")) |> 
  dplyr::select(archive_number, question_id, president, start, end, resp_txt, resp_pct)

# and this shows the same observations in the wide data frame. 
# Note how `unsure` is NA for each. 
roper_toplines_wide |> 
  dplyr::filter(archive_number %in% c("31089821", "31089745", "31088524")) |> 
  dplyr::select(archive_number, question_id, president, start, end, 
         approve, disapprove, unsure, inap)

# correct the three observations from NA to the actual response percentage value
roper_toplines_wide <- roper_toplines_wide |> 
  dplyr::mutate(unsure = dplyr::case_when(
    archive_number == "31089821" ~ 3,
    archive_number == "31089745" ~ 6,
    archive_number == "31088524" ~ 6,
    .default = as.integer(unsure)))

# In the `roper` data, I have a variable column for `month` derived from the
# `start` date of each Gallup poll. I can create a `quarter` column from the
# `start` date column as well. These two variables would allow me to later
# create quarterly and monthly averages of presidential approval. This may also
# align to the quarterly and monthly values of the consumer sentiment index.

# average approval over time by different time intervals (year, quarter, month)
roper_toplines_wide <- roper_toplines_wide |>
  dplyr::mutate(quarter = lubridate::quarter(start, type = "quarter")) |> 
  dplyr::relocate(quarter, .after = year) |> 
  # dplyr::select(year, quarter, month, president, approve) |> 
  dplyr::arrange(year)

# Final n = 1,879
# year range = 1938-2025 (up to March 2025)
roper_toplines_wide |> 
  dplyr::summarise(
    n = dplyr::n(),
    earliest_year = min(year),
    latest_year = max(year)
  )

# number of observations per president
roper_toplines_wide |> 
  dplyr::reframe(n = dplyr::n(), .by=president)


# merge 2015 and 2016 Obama ratings ---------------------------------------

# The Roper Center's iPoll Data base does not contain Gallup Org approval
# ratings data for Obama for the year 2016. This is odd because there Gallup
# polling trend data is presented on the following URL,
# (https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx)

# get approval ratings for Obama in 2016 ----------------------------------

# load `rvest` R package
library(rvest)

# this reads all tables on the page and returns them in a list
# you can save those tables, and then use magrittr to extract specific ones

gallup_obama_tables <-
  rvest::read_html(
    "https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx") %>%
  rvest::html_table(fill = TRUE, header = TRUE) 
# fill = TRUE fills in missing values with NA
# header = TRUE indicates that the first row contains the header names.

# save as rds to check out later
# I'm saving it in alternate directory on my local computer
# readr::write_rds(gallup_obama_tables, file = "~/R/data/Gallup/Obama_Gallup_tables.rds")

# this will output the first table in the console as a tibble
# if you want a different table further down the web page,
# then you have to specify the nth table
obama2016 <- gallup_obama_tables %>%
  magrittr::extract2(2) |>
  dplyr::select(
    date = 1,
    approve = Approve,
    disapprove = Disapprove,
    no_opinion = 'No opinion'
  ) |>
  dplyr::filter(!date == "" & !date == "Gallup") |>
  dplyr::mutate(across(c(approve, disapprove, no_opinion), ~ as.numeric(.)))

obama2016 <- obama2016 |>
  tidyr::separate_wider_delim(cols = date,
                              delim = "-",
                              names_sep = "_")

obama2016 <- obama2016 |>
  tidyr::separate_wider_delim(
    cols = date_1,
    delim = " ",
    names = c("year", "start_month", "start_day")
  )


obama2016 <- obama2016 |>
  dplyr::mutate(start = lubridate::ymd(stringr::str_glue("{year}-{start_month}-{start_day}")),
                .before = date_2)

obama2016 <- obama2016 |>  
  dplyr::mutate(date_2 = dplyr::case_when(
    date_2 == "2017 Jan 1" ~ "Jan 1",
    .default = as.character(date_2)
  ))


obama2016 <- obama2016 |> 
  tidyr::separate_wider_regex(
    cols = date_2,
    patterns = c(end_month = "\\D*", end_day = "\\d+"))

obama2016 <- obama2016 |>
  dplyr::mutate(
    end_month = dplyr::case_when(
      end_month == "" ~  dplyr::coalesce(start_month, end_month),
      .default = as.character(end_month)
  ))

obama2016 <- obama2016 |> 
  dplyr::mutate(
    end = lubridate::ymd(stringr::str_glue("{year}-{end_month}-{end_day}")), 
    .after = start
  )

obama2016 <- obama2016 |> 
  dplyr::mutate(end = dplyr::case_when(
    end == "2016-01-01"~ lubridate::ymd("2017-01-01"),
    .default = end
  ))

obama2016 <- obama2016 |> 
  dplyr::arrange(desc(start)) |> 
  dplyr::select(-year, -start_month, -start_day, -end_month, -end_day)

obama2016 <- obama2016 |> 
  dplyr::mutate(president = rep_len(c("Obama"), length.out = dplyr::n())) |> 
  dplyr::relocate(president, .before = start)


# 2015 --------------------------------------------------------------------


obama2015 <- gallup_obama_tables %>%
  magrittr::extract2(3) |> 
  dplyr::select(date = 1, 
                approve = Approve, 
                disapprove = Disapprove, 
                no_opinion = 'No opinion') |> 
  dplyr::filter(!date == "" & !date == "Gallup") |>
  dplyr::mutate(across(c(approve, disapprove, no_opinion), ~as.numeric(.)))

obama2015 |> print(n = Inf)


obama2015 <- obama2015 |> 
  tidyr::separate_wider_delim(
    cols = date, 
    delim = "-", 
    names_sep = "_")

obama2015 <- obama2015 |>
  tidyr::separate_wider_delim(
    cols = date_1,
    delim = " ",
    names = c("year", "start_month", "start_day")
  )


obama2015 <- obama2015 |>
  dplyr::mutate(start = lubridate::ymd(stringr::str_glue("{year}-{start_month}-{start_day}")),
         .before = date_2)

obama2015 <- obama2015 |>  
  dplyr::mutate(date_2 = dplyr::case_when(
    date_2 == "2016 Jan 3" ~ "Jan 3",
    .default = as.character(date_2)
  ))


obama2015 <- obama2015 |> 
  tidyr::separate_wider_regex(
    cols = date_2,
    patterns = c(end_month = "\\D*", end_day = "\\d+"))

obama2015 <- obama2015 |>
  dplyr::mutate(
    end_month = dplyr::case_when(
      end_month == "" ~  dplyr::coalesce(start_month, end_month),
      .default = as.character(end_month)
  ))

obama2015 <- obama2015 |> 
  dplyr::mutate(
    end = lubridate::ymd(stringr::str_glue("{year}-{end_month}-{end_day}")), 
    .after = start
  )

obama2015 <- obama2015 |> 
  dplyr::mutate(end = dplyr::case_when(
    end == "2015-01-03"~ lubridate::ymd("2016-01-03"),
    .default = end
  ))

obama2015 <- obama2015 |> 
  dplyr::arrange(desc(start)) |> 
  dplyr::select(-year, -start_month, -start_day, -end_month, -end_day)

obama2015 <- obama2015 |> 
  dplyr::mutate(president = rep_len(c("Obama"), length.out = dplyr::n())) |> 
  dplyr::relocate(president, .before = start)


# combine 2016 and 2015 tables
obama_tabs <- obama2016 |> dplyr::bind_rows(obama2015)

obama_tabs <- obama_tabs |> dplyr::rename(unsure = no_opinion)

obama_tabs <- obama_tabs |> dplyr::mutate(
  survey_org = rep_len(c("Gallup Organization"), length.out = dplyr::n()),
  source_doc = rep_len(c("Gallup Poll"), length.out = dplyr::n()),
  question_txt = rep_len(
    c(
      "Do you approve or disapprove of the way Barack Obama is handling his job as president?"
    ),
    length.out = dplyr::n()
  ),
  citation = rep_len(
    c(
      "https://news.gallup.com/poll/116479/barack-obama-presidential-job-approval.aspx"
    ),
    length.out = dplyr::n()
  )
)

obama_tabs <- obama_tabs |> 
  dplyr::mutate(
    year = lubridate::year(start), 
    month = lubridate::month(start, label = T, abbr = T),
    quarter = lubridate::quarter(start, type = "quarter"),
    .before = approve
  )


# clear memory
rm(obama2015, obama2016, gallup_obama_tables)

# join data frames --------------------------------------------------------

# join data frames together
roper_toplines_wide <- roper_toplines_wide |> 
  dplyr::full_join(obama_tabs) |> 
  dplyr::arrange(start)



# Save response topline data set :::::::::::::::::::::::::::::::::::::::::::####

readr::write_rds(roper_toplines_wide, file = "data/roper/roper_toplines_pres_approval.rds")

rm(list = ls())









