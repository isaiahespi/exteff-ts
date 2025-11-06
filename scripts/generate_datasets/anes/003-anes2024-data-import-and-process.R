# This script imports and processes a subset of data derived from the 2024 ANES
# survey. Multiple variables are created (e.g., `age_cat`) or modified. The
# dataset is filtered to include only those who completed both the pre- and
# post-election survey

# citation:
# American National Election Studies. 2025. ANES 2024 Time Series
# Study Full Release [dataset and documentation]. August 8, 2025
# version. www.electionstudies.org

# set up ------------------------------------------------------------------

# load some custom functions
source(here::here('utils', 'funs.R'))

# packages
# library(tidyverse)
# library(fs)
# library(censusapi)
# library(survey) # Analysis of Complex survey samples
# library(srvyr) # 'dplyr'-Like Syntax for summary statistics of survey data


# import raw ANES 2024 data set
anes2024_raw <- haven::read_sav("data-raw/anes/anes_timeseries_2024_spss_20250808.sav")


# 2024 ANES variables -----------------------------------------------------

# subset ANES 2024 data to select variables
anes2024 <- anes2024_raw |>
  # include only respondents who completed pre- and post interview
  # dplyr::filter(V240002c == 2) |>
  # subset to select variables
  dplyr::select(
    version,               
    V200001,               
    V240001,               
    V160001_orig,               
    V240002c,               
    V240002a,               
    V240002b,               
    V240003,               
    V240101a,               
    V240101b,               
    V240101c,               
    V240101d,               
    V240102a,               
    V240102b,               
    V240102c,               
    V240102d,               
    V240103a,               
    V240103b,               
    V240103c,               
    V240103d,               
    V240104a,               
    V240104b,               
    V240104c,               
    V240104d,               
    V240105a,               
    V240105b,               
    V240105c,               
    V240105d,               
    V240106a,               
    V240106b,               
    V240106c,               
    V240106d,               
    V240107a,               
    V240107b,               
    V240107c,               
    V240107d,               
    V240108a,               
    V240108b,               
    V240108c,               
    V240108d,               
    V241177,               
    V241178,               
    V241180,               
    V241181,               
    V241182,               
    V241183,               
    V241184,               
    V241215,               
    V241216,               
    V241217,               
    V241218x,               
    V241219,               
    V241220,               
    V241221,               
    V241221z,               
    V241222,               
    V241223,               
    V241224,               
    V241225,               
    V241226,               
    V241227x,               
    V241228,               
    V241229,               
    V241230,               
    V241231,               
    V241232,               
    V241233,               
    V241234,               
    V241235,               
    V241314,               
    V241315,               
    V241316,               
    V241317,               
    V241318,               
    V241319x,               
    V241320,               
    V241321,               
    V241322x,               
    V241323,               
    V241459,               
    V241460,               
    V241461x,               
    V241462,               
    V241457,               
    V241458x,               
    V241463,               
    V241464,               
    V241465x,               
    V241466,               
    V241467,               
    V241468,               
    V241469x,               
    V241470,               
    V241501x,               
    V241502,               
    V241506,               
    V241507,               
    V241508,               
    V241512x,               
    V241529,               
    V241534,               
    V241536,               
    V241537,               
    V241538,               
    V241550,               
    V241551,               
    V241566x,               
    V241567x,               
    V242004,               
    V242005,               
    V242007x,               
    V242008,               
    V242009,               
    V242010,               
    V242011,               
    V242012,               
    V242013,               
    V242014,               
    V242015,               
    V242020,               
    V242024,               
    V242025,               
    V242026,               
    V242027,               
    V242028,               
    V242029,               
    V242030,               
    V242031,               
    V242032,               
    V242033,               
    V242034,               
    V242035,               
    V242036,               
    V242037,               
    V242051,               
    V242052,               
    V242053,               
    V242054x,               
    V242055,               
    V242057a,               
    V242057b,               
    V242058x,               
    V242059,               
    V242060a,               
    V242060b,               
    V242061,               
    V242062,               
    V242091x,               
    V242092x,               
    V242093x,               
    V242094x,               
    V242095x,               
    V242096x,               
    V242097x,               
    V242098x,               
    V242099x,               
    V242100a,               
    V242100b,               
    V242100c,               
    V242100d,               
    V242100e,               
    V242100f,               
    V242100g,               
    V242100h,               
    V242100i,               
    V242100j,               
    V242100k,               
    V242101,               
    V242104x,               
    V242107x,               
    V242110x,               
    V242113x,               
    V242114a,               
    V242114b,               
    V242114c,               
    V242114d,               
    V242114e,               
    V242114f,               
    V242114g,               
    V242114h,               
    V242114i,               
    V242114j,               
    V242114k,               
    V242114m,               
    V242115,               
    V242116,               
    V242200,               
    V242201,               
    V242202,               
    V242203,               
    V242204,               
    V242205,               
    V242206,               
    V242207,               
    V242208,               
    V242209,               
    V242210,               
    V242211,               
    V242212,               
    V242213,               
    V242214,               
    V242215,               
    V242216,               
    V242217,               
    V242218,               
    V242219,               
    V242220x,               
    V242221,               
    V242222,               
    V242223x,               
    V242224,               
    V242225,               
    V242226x,               
    V242227,               
    V242228,               
    V242229,               
    V242230,               
    V242231x,               
    V242232,               
    V242233,               
    V242234x,               
    V242235,               
    V242236,               
    V242237,               
    V242238,               
    V242239,               
    V242240,               
    V242241x,               
    V242242,               
    V242243,               
    V242244,               
    V242245x,               
    V242246,               
    V242247,               
    V242248x,               
    V242249,               
    V242250,               
    V242251,               
    V242252,               
    V242253x,               
    V242254,               
    V242255,               
    V242256,               
    V242257,               
    V242258,               
    V242259,               
    V242260,               
    V242261,               
    V242262,               
    V242263,               
    V242264,               
    V242265,               
    V242266x,               
    V242267,               
    V242268,               
    V242269x,               
    V242270,               
    V242271,               
    V242272x,               
    V242273,               
    V242274,               
    V242275x,               
    V242276,               
    V242277,               
    V242278,               
    V242279x,               
    V242280,               
    V242281,               
    V242300,               
    V242301,               
    V242302,               
    V242303,               
    V242304,               
    V242305,               
    V242306,               
    V242307,               
    V242308x,               
    V242309,               
    V242310,               
    V242311,               
    V242312,               
    V242313,               
    V242314x,               
    V242315,               
    V242316a,               
    V242316b,               
    V242317,               
    V242318,               
    V242319x,               
    V242320,               
    V242321,               
    V242322,               
    V242323,               
    V242324x,               
    V242325,               
    V242326,               
    V242327,               
    V242328x,               
    V242329,               
    V242330,               
    V242331x,               
    V242332,               
    V242333,               
    V242334,               
    V242335x,               
    V242336,               
    V242337,               
    V242338,               
    V242339,               
    V242340,               
    V242341,               
    V242342,               
    V242343,               
    V242344,               
    V242345,               
    V242346x,               
    V242347,               
    V242348,               
    V242349x,               
    V242350,               
    V242351,               
    V242352,               
    V242353x,               
    V242354,               
    V242355,               
    V242356,               
    V242357x,               
    V242358,               
    V242359,               
    V242360,               
    V242361x,               
    V242362,               
    V242363,               
    V242364x,               
    V242365,               
    V242366,               
    V242367,               
    V242368,               
    V242369,               
    V242370,               
    V242401,               
    V242402,               
    V242403,               
    V242404,               
    V242405,               
    V242406,               
    V242407,               
    V242408,               
    V242409,               
    V242410,               
    V242411,               
    V242412,               
    V242413,               
    V242414,               
    V242415,               
    V242416,               
    V242417,               
    V242418,               
    V242419,               
    V242420,               
    V242421,               
    V242422,               
    V242423,               
    V242424,               
    V242425,               
    V242426,               
    V242427,               
    V242428,               
    V242429,               
    V242430,               
    V242431,               
    V242432,               
    V242433,               
    V242434,               
    V242435,               
    V242436,               
    V242437,               
    V242438,               
    V242439,               
    V242440,               
    V242441,               
    V242442,               
    V242443,               
    V242444,               
    V242445,               
    V242446,               
    V242447,               
    V242448,               
    V242449,               
    V242450,               
    V242451,               
    V242452,               
    V242453,               
    V242454,               
    V242455,               
    V242456,               
    V242458x,               
    V242459,               
    V242460x,               
    V242461,               
    V242500a,               
    V242500b,               
    V242500c,               
    V242501,               
    V242502,               
    V242503,               
    V242504,               
    V242505,               
    V242506,               
    V242507,               
    V242508,               
    V242509a,               
    V242509b,               
    V242509c,               
    V242509d,               
    V242509e,               
    V242509f,               
    V242510,               
    V242511a,               
    V242511b,               
    V242511c,               
    V242511d,               
    V242511e,               
    V242511f,               
    V242512,               
    V242513,               
    V242514,               
    V242515,               
    V242516,               
    V242517,               
    V242518,               
    V242519,               
    V242520,               
    V242521,               
    V242522x,               
    V242523,               
    V242524,               
    V242525x,               
    V242526,               
    V242527,               
    V242528,               
    V242529,               
    V242536,               
    V242537,               
    V242538,               
    V242539,               
    V242540,               
    V242541,               
    V242542,               
    V242543,               
    V242544,               
    V242545,               
    V242546,               
    V242547,               
    V242548,               
    V242549,               
    V242550,               
    V242551,               
    V242552,               
    V242553,               
    V242554,               
    V242555,               
    V242556,               
    V242557,               
    V242558,               
    V242559,               
    V242560,               
    V242561,               
    V242562,               
    V242563,               
    V242564,               
    V242565,               
    V242566,               
    V242567,               
    V242568,               
    V242569,               
    V242570,               
    V242571,               
    V242572,               
    V242573,               
    V242574,               
    V242575,               
    V242576,               
    V242577a,               
    V242577b,               
    V242577c,               
    V242577d,               
    V242577e,               
    V242577f,               
    V242577g,               
    V242577h,               
    V242578,               
    V242579,               
    V242580,               
    V242581,               
    V242600,               
    V242601,               
    V242603,               
    V242603z,               
    V242604,               
    V242605,               
    V242606,               
    V242607,               
    V243001,               
    V243002,               
    V243007,               
    V243008,               
    V243009,               
    V243050,               
    V243051,               
    V243052,               
    V243053,               
    V243054,               
    V243055,               
    V243059,               
    V243056,               
    V243057,               
    V243058,               
    V243060,               
    V243061,               
    V243062,               
    V243063,               
    V243064,               
    V243065,               
    V243066,               
    V243067            
  )


# Optional
# this gets rid of the the annoying prefix embedded in the value labels of ANES
# variables, e.g., "1. Response"
# If no `.col` are specified, then applies to the whole data set
anes2024 <- anes2024 |> 
  labelled::update_value_labels_with(
    .fn = ~stringr::str_remove(., "^-*[0-9]+[\\.]\\s*")
    )

# rename select variables  -------------------------------------

# make a codebook and add column of ANES variable codes for reference
anes2024_codebook <- codebook(anes2024) |>
  dplyr::mutate(ANES_var = stringr::str_to_sentence(stringr::str_glue("{variable}")),
                .before = label)

# optional
# rename select columns (not all columns are renamed)
anes2024 <- anes2024 |> 
  dplyr::rename(
  caseid2020       = V200001,      # 2020 Time Series Case ID
  caseid           = V240001,      # 2024 Time Series Case ID
  caseid2016       = V160001_orig, # 2016 Time Series Case ID
  complete_status  = V240002c,    # Pre/Post Interview Completion Status
  intv_mode_pre    = V240002a,    # Mode of interview: pre-election interview
  intv_mode_post   = V240002b,    # Mode of interview: post-election interview
  sample_type      = V240003,     # Sample Type
  ideo             = V241177,     # PRE: 7pt scale liberal-conservative self-placement
  ideo_choose      = V241178,     # PRE: If R had to choose liberal or conservative self-placement
  ideo_repcand     = V241180,     # PRE: 7pt scale liberal-conservative: Republican Presidential candidate
  ideo_demhscand   = V241181,     # PRE: 7pt scale liberal-conservative: Democratic House candidate
  ideo_rephscand   = V241182,     # PRE: 7pt scale liberal-conservative: Republican House candidate
  ideo_demparty    = V241183,     # PRE: 7pt scale liberal-conservative: Democratic party
  ideo_repparty    = V241184,     # PRE: 7pt scale liberal-conservative: Republican party
  duty_v1a         = V241215,     # PRE: VERSION 1A Does R consider voting a duty or choice
  duty_str1        = V241216,     # PRE: How strongly does R feel that voting is a duty
  duty_str2        = V241217,     # PRE: How strongly does R feel that voting is a choice
  duty             = V241218x,    # PRE: SUMMARY: Voting as duty or choice
  splitticket      = V241219,     # PRE: Split-ticket voting
  splitgov         = V241220,     # PRE: Party Control or split government
  partyid          = V241227x,    # PRE: SUMMARY: Party ID
  partid_imp       = V241228,     # PRE: Party identity importance
  trustgov1        = V241229,     # PRE: How often trust government in Washington to do what is right [revised]
  trustcrt         = V241230,     # PRE: How often can you trust the court system to do what is right
  trustgov2        = V241231,     # PRE: Government run by a few big interests or for benefit of all
  trustgov3        = V241232,     # PRE: Does government waste much tax money
  trustgov4        = V241233,     # PRE: How many in government are corrupt
  trustppl         = V241234,     # PRE: How often can people be trusted
  govresp          = V241235,     # PRE: Elections make government pay attention
  votecount        = V241314,     # PRE: Votes counted accurately
  trustelectnoff   = V241315,     # PRE: Trust election officials
  votestop         = V241316,     # PRE: How often people denied right to vote
  showid           = V241319x,    # PRE: SUMMARY: Favor/oppose requiring ID when voting
  felonvote        = V241322x,    # PRE: SUMMARY: Favor/oppose allowing felons to vote
  voteidstate      = V241323,     # PRE: Does R's state ask for ID to vote
  age              = V241458x,    # PRE: SUMMARY: Respondent age on Election Day
  martial          = V241461x,    # PRE: SUMMARY: Marital status (5 category)
  partner          = V241462,     # PRE: Domestic partnership status
  educ             = V241463,     # PRE: Highest level of education
  educ_papi        = V241464,     # PRE: PAPER: Highest level of education
  educ5            = V241465x,    # PRE: SUMMARY: Respondent 5 Category level of education
  educ_dipged      = V241466,     # PRE: R: Diploma or GED
  edspouse         = V241467,     # PRE: Spouse partner: Highest level of education
  edspouse_dipged  = V241468,     # PRE: Spouse: Diploma or GED
  edspouse5        = V241469x,    # PRE: SUMMARY: Respondent spouse/partner 5 Category level of education
  mil              = V241470,     # PRE: Armed forces active duty
  race             = V241501x,    # PRE: SUMMARY: R self-identified race/ethnicity
  race_mena        = V241502,     # PRE: R: Are you of Middle Eastern or North African descent
  parents          = V241506,     # PRE: Native status of parents
  birthplace       = V241507,     # PRE: Rs: born US, Puerto Rico, or some other country
  cit_papi         = V241508,     # PRE: PAPER: R citizen of US or another country
  hisp             = V241512x,    # PRE: SUMMARY: Hispanic summary
  whereraised      = V241529,     # PRE: Where R grew up
  residentyrs      = V241534,     # PRE: Years R lived at address
  drvlic           = V241536,     # PRE: Does R have a valid, non-expired driver's license
  passport         = V241537,     # PRE: Does R have a valid, non-expired passport
  othergovid       = V241538,     # PRE: Does R have another form of ID
  sex              = V241550,     # PRE: What is R's sex? [revised]
  gender           = V241551,     # PRE: What is R's gender?
  income           = V241566x,    # PRE: SUMMARY: Total (household) income
  income6          = V241567x,    # PRE: SUMMARY: Total (household) income [six category]
  party_contacted  = V242007x,    # POST: SUMMARY: Which party contact R about campaign
  other_contacted  = V242008,     # POST: Someone not from two major parties contact R about supporting candidates
  mobreg           = V242009,     # POST: Anyone talk to R about registering or getting out to vote
  persuade_others  = V242010,     # POST: R talk to anyone about voting for or against a party or candidate
  rally_online     = V242011,     # POST: R attend online political meetings, rallies, speeches, fundraisers
  attend_rally     = V242012,     # POST: R go to any political meetings, rallies, speeches, dinners
  wearbutton       = V242013,     # POST: R wear campaign button or post sign or bumper sticker
  campgnwrk        = V242014,     # POST: R do any (other) work for party or candidate
  donate_cand      = V242015,     # POST: Did R contribute money to individual candidate running for public office
  donate_party     = V242020,     # POST: R contribute money to political party during this election year
  dontate_othr     = V242024,     # POST: R contribute to any other group that supported or opposed candidates
  talk_politics    = V242025,     # POST: How many days in past week discussed politics with family or friends
  talk_childrace   = V242026,     # POST: Does R discuss race or racial issues with children/teenagers in household
  talk_childpol    = V242027,     # POST: Does R discuss government and politics with children/teenagers in household
  talk_intldisc    = V242028,     # POST: Does R talk to...who live in other countries about events in that country
  involv12_rally   = V242029,     # POST: Has R in past 12 months: joined a protest march, rally, or demonstration
  involv12_signpet = V242030,     # POST: Has R in past 12 months: sign internet or paper petition
  involv12_givsoc  = V242031,     # POST: Has R in past 12 months: given money to other organization
  involv12_postint = V242032,     # POST: Has R in past 12 months: posted comment online about political issue
  involv12_poliarg = V242033,     # POST: Has R in past 12 months: gotten into a political argument with someone
  involv12_loclmtg = V242034,     # POST: Has R in past 12 months: attend mtg about issue facing community/schools
  involv12_voltwrk  = V242035,    # POST: Has R in past 12 months: done any volunteer work
  involv12_cntctrep = V242036,    # POST: Has R in past 12 months: contacted federal elected official
  involv12_boycott = V242037,     # POST: How often bought or boycotted product/service for social/political reasons
  registered       = V242051,     # POST: Is R registered to vote
  whenregister     = V242054x,    # POST: SUMMARY: When did R register to vote for the first time
  postvote_addr    = V242055,     # POST: Verify R's address
  postvote_newaddr = V242057a,    # POST: Registration address given (not registered at sample address)
  postvote_newwhst = V242057b,    # POST: State of R's voting address (not registered at sample address)
  st_regis         = V242058x,    # PRE-POST: SUMMARY: State of registration (all registered voters)
  st_partyregis    = V242061,     # POST: Party registration in state of registration (pre nonvoter)
  postvote_regyrs  = V242062,     # POST: How long has R been registered at voting address
  vote_votepref    = V242091x,    # POST: SUMMARY:POST-ELECTION PRESIDENTIAL VOTE/PREFERENCE
  vote2024_hs      = V242092x,    # POST: SUMMARY:PARTY OF POST-ELECTION US HOUSE VOTE/PREFERENCE
  vote2024_sen     = V242093x,    # POST: SUMMARY:PARTY OF POST-ELECTION US SENATE VOTE/PREFERENCE
  vote2024_govnr   = V242094x,    # POST: SUMMARY:PARTY OF POST-ELECTION GUBERNATORIAL VOTE/PREFERENCE
  turnout2024      = V242095x,    # PRE-POST: SUMMARY: VOTER TURNOUT IN 2024
  vote_pres        = V242096x,    # PRE-POST: SUMMARY: 2024 PRESIDENTIAL VOTE
  vote_hs          = V242097x,    # PRE-POST: SUMMARY: PARTY OF 2024 US HOUSE VOTE
  vote_sen         = V242098x,    # PRE-POST: SUMMARY:  PARTY OF 2024 US SENATE VOTE
  vote_govnr       = V242099x,    # PRE-POST: SUMMARY: PARTY OF 2024 GOVERNOR VOTE
  nonreg_reason1     = V242100a,  # POST: Reason R is not registered - did not meet registration deadlines
  nonreg_reason2     = V242100b,  # POST: Reason R is not registered - did not know where or how to register
  nonreg_reason3     = V242100c,  # POST: Reason R is not registered - did not meet residency requirements
  nonreg_reason4     = V242100d,  # POST: Reason R is not registered - registration form was not processed correctly
  nonreg_reason5     = V242100e,  # POST: Reason R is not registered - did not have required identification
  nonreg_reason6     = V242100f,  # POST: Reason R is not registered - not interested in the election
  nonreg_reason7     = V242100g,  # POST: Reason R is not registered - my vote would not make a difference
  nonreg_reason8     = V242100h,  # POST: Reason R is not registered - permanent illness or disability
  nonreg_reason9     = V242100i,  # POST: Reason R is not registered - difficulty with English
  nonreg_reason10    = V242100j,  # POST: Reason R is not registered - not eligible to vote
  nonreg_reason11    = V242100k,  # POST: Reason R is not registered - other
  voteexp_whenvote   = V242104x,  # POST: SUMMARY: When R voted in 2024 election
  voteexp_howvote    = V242107x,  # POST: SUMMARY: How R voted in 2024 election
  voteexp_usualvote  = V242110x,  # POST: SUMMARY: How does R usually vote
  voteexp_votehard   = V242113x,  # POST: SUMMARY: How difficult was it for R to vote
  voteexp_voteprob1  = V242114a,  # POST: Did R encounter any problems voting - registration problem
  voteexp_voteprob2  = V242114b,  # POST: Did R encounter any problems voting - concern about identification card
  voteexp_voteprob3  = V242114c,  # POST: Did R encounter any problems voting - difficulty obtaining absentee ballot
  voteexp_voteprob4  = V242114d,  # POST: Did R encounter any problems voting - confusion about ballot or machine
  voteexp_voteprob5  = V242114e,  # POST: Did R encounter any problems voting - difficulty getting to polling place
  voteexp_voteprob6  = V242114f,  # POST: Did R encounter any problems voting - long wait times
  voteexp_voteprob7  = V242114g,  # POST: Did R encounter any problems voting - work schedule
  voteexp_voteprob8  = V242114h,  # POST: Did R encounter any problems voting - bad weather
  voteexp_voteprob9  = V242114i,  # POST: Did R encounter any problems voting - issue mailing ballot
  voteexp_voteprob10 = V242114j,  # POST: Did R encounter any problems voting - other problem
  voteexp_voteprob11 = V242114k,  # POST: Did R encounter any problems voting - none at all
  voteexp_voteprob12 = V242114m,  # POST: Did R encounter any problems voting - R didn't try to vote
  voteexp_waittime   = V242115,   # POST: How long was wait time at polling place
  voteexp_trvltime   = V242116,   # POST: How long does it take to get to polling place
  nocare             = V242200,   # POST: [STD] Public officials don't care what people think
  nosay              = V242201,   # POST: [STD] Have no say about what government does
  complex            = V242202,   # POST: [REV] Politics/government too complicated to understand
  understand         = V242203,   # POST: [REV] How well does R understand important political issues
  electn_faircounts  = V242207,   # POST: How often are votes counted fairly
  electmore_hisp     = V242208,   # POST: How important that more Hispanics get elected to political office
  electmore_blacks   = V242209,   # POST: How important that more blacks get elected to political office
  electmore_asians   = V242210,   # POST: How important that more Asians get elected to political office
  electmore_lgbt     = V242211,   # POST: How important that more LGBT people get elected to political office
  electmore_women    = V242212,   # POST: How important that more women get elected to political office
  womenpres_elect    = V242213,   # POST: How important that a woman be elected President of the US
  campfin_spendlim   = V242214,   # POST: Limits on campaign spending
  campfin_org        = V242215,   # POST: Congress pass laws that benefit contributor organization
  campfin_direct     = V242216,   # POST: Congress pass laws that benefit contributor individuals
  campfin_donate     = V242217,   # POST: Congress change votes because of donation to campaign
  courtfair_trmp     = V242220x,  # POST: SUMMARY: Have the courts treated Donald Trump fairly or unfairly
  elite_influence    = V242304,   # POST: Our political system only works for insiders with money and power
  elite_ahead        = V242305,   # POST: Because of rich and powerful it's difficult for the rest to get ahead
  victim_fairshare   = V242308x,  # POST: SUMMARY: Does R think they get more or less than their fair share in life
  gendid_gendmasc    = V242309,   # POST: R masculinity scale
  gendid_gendfem     = V242310,   # POST: R femininity scale
  oppurtunity        = V242311,   # POST: How much opportunity in America for average person to get ahead
  econmobility       = V242314x,  # POST: SUMMARY: Economic mobility
  cses_understand    = V242408,   # POST: CSES6-Q03: Agree/disagree: R understands most important political issues
  cses_prefdemoc     = V242409,   # POST: CSES6-Q04a: Prefer democracy
  cses_courts        = V242410,   # POST: CSES6-Q04b: Courts' authority
  cses_strlead       = V242411,   # POST: CSES6-Q04e: Attitudes about elites: strong leader in government is good
  cses_repwomen      = V242412,   # POST: CSES6-Q04d: Policies to increase women representation
  cses_insteadbus    = V242413,   # POST: CSES6-Q05a: Political decisions should be left to business leaders
  cses_insteadexp    = V242414,   # POST: CSES6-Q05b: Political decisions should be left to independent experts
  cses_insteadcit    = V242415,   # POST: CSES6-Q05bc: Political decisions should be left to citizen referendums
  cses_democratic    = V242416,   # POST: CSES6_Q06: US democracy scale
  cses_trustcong     = V242417,   # POST: CSES6-Q07a: Trust congress
  cses_trustgov      = V242418,   # POST: CSES6-Q07b: Trust government
  cses_trustjud      = V242419,   # POST: CSES6-Q07c: Trust judiciary
  cses_trustsci      = V242420,   # POST: CSES6-Q07d: Trust scientists
  cses_trustparties  = V242421,   # POST: CSES6-Q07e: Trust political parties
  cses_trustmedia    = V242422,   # POST: CSES6-Q07f: Trust traditional media
  cses_trustsocmedia = V242423,   # POST: CSES6-Q07g: Trust social media
  cses_presperform   = V242424,   # POST: CSES6-Q09: How good/bad a job has government done in last 4 years
  cses_prescovid     = V242425,   # POST: CSES6-Q08b: President performance COVID
  cses_econ          = V242426,   # POST: CSES6-Q11: State of economy better or worse over past 12 months
  cses_votesat       = V242427,   # POST: CSES6-Q11a: Satisfaction about vote: voters for party/candidate
  cses_novote        = V242428,   # POST: CSES6-Q11c: Satisfaction about vote: non-voters
  cses_electchoice   = V242429,   # POST: CSES6-Q12: Satisfaction about variety of choice
  cses_fairelect     = V242430,   # POST: CSES6-Q13: Fairness of election
  cses_votediff      = V242431,   # POST: CSES6-Q14b: Who people vote for makes a big difference
  cses_lddempty      = V242432,   # POST: CSES6-Q15a: Like-dislike-Democratic party
  cses_ldreppty      = V242433,   # POST: CSES6-Q15b: Like-dislike-Republican Party
  cses_lddpc         = V242434,   # POST: CSES6-Q16a: Like-dislike-Democratic Presidential candidate
  cses_ldrpc         = V242435,   # POST: CSES6-Q16b: Like-dislike-Republican Presidential candidate
  cses_lrdpc         = V242436,   # POST: CSES6-Q17a: Left-right-Democratic Party
  cses_lrrpc         = V242437,   # POST: CSES6-Q17b: Left-right-Republican Party
  cses_lrself        = V242438,   # POST: CSES6-Q18: Left-right-self
  cses_demsatis      = V242439,   # POST: CSES6-Q21: Satisfaction with democratic process
  cses_immcrime      = V242453,   # POST: CSES6- Q05c: Out-group attitudes: Immigrants increase crime
  cses_amerborn      = V242454,   # POST: CSES6- Q06a: National identity: to have been born in country
  cses_amereng       = V242455,   # POST: CSES6- Q06c: National identity: to be able to speak English
  cses_polcorrupt    = V242456,   # POST: CSES6- Q07: How widespread is corruption
  transpolicy_sprts  = V242458x,  # PRE-POST: SUMMARY: Does R favor/oppose banning transgender girls from sports
  owngun_gunnum      = V242460x,  # PRE-POST: SUMMARY: How many guns does R own
  ftcasi_asians      = V242514,   # POST: Feeling thermometer: Asian-Americans
  ftcasi_hisp        = V242515,   # POST: Feeling thermometer: Hispanics
  ftcasi_blacks      = V242516,   # POST: Feeling thermometer: blacks
  ftcasi_illimmg     = V242517,   # POST: Feeling thermometer: illegal immigrants
  ftcasi_whites      = V242518,   # POST: Feeling thermometer: whites
  grpconsc_whitejob  = V242519,   # POST: How likely whites unable to find jobs because employers hiring minorities
  grptreat_fedgov    = V242522x,  # POST: SUMMARY: Federal government treats blacks or whites better
  grptreat_police    = V242525x,  # POST: SUMMARY: Police treat blacks or whites better
  infl_white         = V242526,   # POST: How much influence do whites have in US politics
  infl_blacks        = V242527,   # POST: How much influence do blacks have in US politics
  infl_hisp          = V242528,   # POST: How much influence do Hispanics have in US politics
  infl_asians        = V242529,   # POST: How much influence do Asians have in US politics
  ident_amerid       = V242536,   # POST: How important is being American to R's identity
  link_affwhites     = V242537,   # POST: White R: what happens to white people will affect R's life
  link_affhisp       = V242538,   # POST: Hispanic R: what happens to Hispanic people will affect R's life
  link_affblacks     = V242539,   # POST: Black R: what happens to black people will affect R's life
  link_affasians     = V242540,   # POST: Asian R: what happens to Asian people will affect R's life
  stype_hwkwhites    = V242541,   # POST: Stereotype: Whites hardworking
  stype_hwkblacks    = V242542,   # POST: Stereotype: Blacks hardworking
  stype_hwkhisp      = V242543,   # POST: Stereotype: Hispanics hardworking
  stype_hwkasian     = V242544,   # POST: Stereotype: Asian-Americans hardworking
  stype_vlntwhite    = V242545,   # POST: Stereotype: Whites violent
  stype_vlntblack    = V242546,   # POST: Stereotype: Blacks violent
  stype_vlnthisp     = V242547,   # POST: Stereotype: Hispanics violent
  stype_vlntasian    = V242548,   # POST: Stereotype: Asian-Americans violent
  discrim_blacks     = V242549,   # POST: Discrimination in the US against blacks
  discrim_hisp       = V242550,   # POST: Discrimination in the US against Hispanics
  discrim_asians     = V242551,   # POST: Discrimination in the US against Asians
  discrim_whites     = V242552,   # POST: Discrimination in the US against whites
  discrim_gays       = V242553,   # POST: Discrimination in the US against Gays and Lesbians
  discrim_women      = V242554,   # POST: Discrimination in the US against women
  discrim_men        = V242555,   # POST: Discrimination in the US against men
  discrim_musl       = V242556,   # POST: Discrimination in the US against Muslims
  discrim_xtian      = V242557,   # POST: Discrimination in the US against Christians
  discrim_trans      = V242558,   # POST: Discrimination in the US against transgender people
  discrim_personal   = V242559,   # POST: How much discrimination has R faced personally because or race/ethnicity
  discrim_sexharass  = V242560,   # POST: Has R experienced harassment at work
  discrim_sexharassamt = V242561, # POST: How often has R experienced harassment at work
  knowl_emprate      = V242562,   # POST: What is the current unemployment rate
  misinfo_autism     = V242563,   # POST: Does most scientific evidence show vaccines cause autism or not
  misinfo_autismconf = V242564,   # POST: How confident is R about that (vaccines cause autism)
  misinfo_warm       = V242565,   # POST: Have world temperatures have risen on average or last 100 years or not
  misinfo_warmconf   = V242566,   # POST: How confident is R about that (world temperatures have risen)
  misinfo_win20      = V242567,   # POST: Who was the legitimate winner of the 2020 Presidential election
  misinfo_win20conf  = V242568,   # POST: How confident is R about that (winner of 2020 election)
  misinfo_hunter     = V242569,   # POST: Hunter Biden's laptop showed he made deal with...
  misinfo_hunterconf = V242570,   # POST: How confident is R about that (Hunter Biden)
  misinfo_jansix     = V242571,   # POST: Events on January 6, 2021 were carried out by...
  misinfo_jansixconf = V242572,   # POST: How confident is R about that (events on January 6, 2021)
  misinfo_trump16     = V242573,  # POST: Donald Trump's campaign colluded with Russian government in 2016
  misinfo_trump16conf = V242574,  # POST: How confident is R about that (Trump colluded with Russia)
  misinfo_win24       = V242575,  # POST: Was Trump or Harris the legitimate winner of the 2024 Presidential election
  misinfo_win24conf   = V242576,  # POST: How confident is R about that (winner of 2024 election)
  socmedia_facebk     = V242577a, # POST: Which social media platforms R visited - Facebook [mention]
  socmedia_twitter    = V242577b, # POST: Which social media platforms R visited - Twitter [mention]
  socmedia_insta      = V242577c, # POST: Which social media platforms R visited - Instagram [mention]
  socmedia_reddit     = V242577d, # POST: Which social media platforms R visited - Reddit [mention]
  socmedia_youtbe     = V242577e, # POST: Which social media platforms R visited - YouTube [mention]
  socmedia_snapcht    = V242577f, # POST: Which social media platforms R visited - SnapChat [mention]
  socmedia_tiktok     = V242577g, # POST: Which social media platforms R visited - TikTok [mention]
  socmedia_other      = V242577h, # POST: Which social media platforms R visited - Other [mention]
  facebk_frq          = V242578,  # POST: How often use Facebook
  facebk_postpoli     = V242579,  # POST: How often post political content on Facebook
  twitter_frq         = V242580,  # POST: How often use Twitter
  twitter_postpoli    = V242581,  # POST: How often post political content on Twitter
  voteval_list        = V242600,  # POST: VOTE VALIDATION: Do vote validation records match R
  voteval_correct     = V242601,  # POST: VOTE VALIDATION: Personal information correct or not
  voteval_feedback    = V242603,  # POST: VOTE VAL: How comfortable...asked about vote registration information
  st_abb              = V243001,  # SAMPLE: Sample location state postal abbreviation
  st_fips             = V243002,  # SAMPLE: Sample location FIPS state code
  region              = V243007,  # SAMPLE: Census region
  tz                  = V243008,  # SAMPLE: Time zone (majority of  state)
  congdist            = V243009,  # SAMPLE: Sample location Congressional district (119th)
  intv_sdate_pre      = V243050,  # ADMIN: Pre-election interview start date (YYYYMMDD)
  intv_stime_pre      = V243051,  # ADMIN: Pre-election interview start time (HHMMSS)
  intv_edate_pre      = V243052,  # ADMIN: Pre-election interview end date (YYYMMDD)
  intv_etime_pre      = V243053,  # ADMIN: Pre-election interview end time (HHMMSS)
  papi_date_pre       = V243054,  # ADMIN: Completion date pre-election PAPI survey (YYYYMMDD)
  intv_length_pre     = V243055,  # ADMIN: Pre-election interview length (minutes)
  intv_incent_pre     = V243059,  # ADMIN: Pre-election incentive paid
  web_mobile_pre      = V243056,  # ADMIN: Pre-election web device logins: mobile
  web_desktop_pre     = V243057,  # ADMIN: Pre-election web device logins: desktop
  web_tablet_pre      = V243058,  # ADMIN: Pre-election web device logins: tablet
  intv_sdate_post     = V243060,  # ADMIN: Post-election interview start date (YYYYMMDD)
  intv_stime_post     = V243061,  # ADMIN: Post-election interview start time (HHMMSS)
  intv_edate_post     = V243062,  # ADMIN: Post-election interview end date (YYYYMMDD)
  intv_etime_post     = V243063,  # ADMIN: Post-election interview end time (HHMMSS)
  intv_length_post    = V243064,  # ADMIN: Post-election interview length (minutes)
  web_mobile_post      = V243065, # ADMIN: Post-election web device logins: mobile
  web_desktop_post     = V243066, # ADMIN: Post-election web device logins: desktop
  web_tablet_post      = V243067  # ADMIN: Post-election web device logins: tablet
  )


# rename selection of weight variables (not all of them)
# weight variables
# V240101a Pre-election fresh in-person sample alone
# V240101b Post-election fresh in-person sample alone
# V240102a Pre-election fresh web sample alone
# V240102b Post-election fresh web sample alone
# V240103a Pre-election fresh samples (fresh in-person + fresh web)
# V240103b Post-election fresh samples (fresh in-person + fresh web)
# V240104a Pre-election fresh web sample + PAPI
# V240104b Post-election fresh web sample + PAPI
# V240105a Pre-election fresh sample (fresh in-person + fresh web + PAPI)
# V240105b Post-election fresh sample (fresh in-person + fresh web + PAPI)
# V240106a Pre-election panel alone
# V240106b Post-election panel alone
# V240107a Pre-election full sample (fresh in-person + fresh web + panel) + PAPI
# V240107b Post-election full sample (fresh in-person + fresh web + panel) + PAPI
# V240108a Pre-election full sample (fresh in-person + fresh web + panel)
# V240108b Post-election full sample (fresh in-person + fresh web + panel)

# rename weight, stratum, and psu vars
anes2024 <- anes2024 |> 
  dplyr::rename(
    wt_ftf        = V240101b, # Post-election fresh in-person sample alone
    psu_ftf       = V240101c, # PSU: Face-to-Face (FTF) sample [FINAL]
    strata_ftf    = V240101d, # Stratum: Face-to-Face (FTF) sample [FINAL]
    wt_web        = V240104b, # Post-election fresh web sample + PAPI
    psu_web       = V240104c, # PSU: Web sample with PAPI [FINAL]
    strata_web    = V240104d, # Stratum: Web sample with PAPI [FINAL]
    wt_fresh      = V240105b, # Post-election raked weight: Face-to-Face (FTF) & Web samples combined w/PAPI […
    psu_fresh     = V240105c, # PSU: Face-to-Face (FTF) & Web samples combined w/PAPI [FINAL]
    stratum_fresh = V240105d, # Stratum:  Face-to-Face (FTF) & Web samples combined w/PAPI [FINAL]
    wt_panel      = V240106b, # Post-election raked weight: 2016-2024 Panel sample [FINAL]
    psu_panel     = V240106c, # PSU: 2016-2024 Panel sample [FINAL]
    stratum_panel = V240106d, # Stratum: 2016-2024 Panel sample [FINAL]
    wt_full       = V240107b, # Post-election raked weight: 2016-2024 Panel, Face-to-Face (FTF) & Web samples …
    psu_full      = V240107c, # PSU: 2016-2024 Panel, Face-to-Face (FTF) & Web samples combined w/PAPI [FINAL]
    stratum_full  = V240107d  # Stratum: 2016-2024 Panel, Face-to-Face (FTF) & Web samples combined w/PAPI [FI 
  )

# dplyr::glimpse(anes2024)

# In codebook, modify `variable` column to reflect new variable names
anes2024_codebook <- anes2024_codebook |>
  dplyr::mutate(variable = names(anes2024))



# processing ---------------------------------------------------------

# create an `age_cat` factor variable. Also set variable label
anes2024 <- anes2024 |> 
  dplyr::mutate(age_cat = cut(
    x = age,
    c(17, 29, 39, 49, 59, 69, 79, 200),
    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 or older")),
    .after = age) |> 
  sjlabelled::var_labels(age_cat = "Age group")


# I only need those who completed both the pre- and post-election survey
anes2024 <- anes2024 |>
  dplyr::filter(complete_status == 2)

#  Missing data are coded as negative numbers, for instance, “-8. Don’t know”.
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

anes2024 <- anes2024 |>
  dplyr::mutate(dplyr::across(dplyr::where(labelled::is.labelled),
                ~sjlabelled::set_na(., na = na_vals)))

# Alternatively, just NA the specific values
# anes2024 |>
#   dplyr::mutate(
#     dplyr::across(
#       dplyr::where(labelled::is.labelled),
#       ~sjlabelled::set_na(., na = c(-9,-8, -7, -6, -5, -4, -3, -2, -1)))
#     )


# create external efficacy index following ANES CDF coding -------------------

# create an external efficacy index following the coding scheme of the
# American National Election Studies (ANES).

# create recoded variables of NOCARE and NOSAY following ANES CDF coding
# scheme
anes2024 <- anes2024 |>
  dplyr::mutate(dplyr::across(
    c(nocare, nosay),
    ~ dplyr::case_when(.x %in% c(1, 2) ~ 0, .x == 3 ~ 50, .x %in% c(4, 5) ~ 100, TRUE ~ NA),
    .names = "{col}.cdf"
  )) |>
  dplyr::mutate(dplyr::across(c(nocare.cdf, nosay.cdf), ~ labelled::add_value_labels(
    ., c(
      "Agree" = 0,
      "Neither agree nor disagree" = 50,
      "Disagree" = 100
    )
  )))

# create unipolar external efficacy index variable
anes2024 <- anes2024 |> 
  dplyr::rowwise() |> 
  dplyr::mutate(exteff.cdf = mean(c(nocare.cdf, nosay.cdf), na.rm = T)) |>
  # dplyr::mutate(exteff.cdf = sum(dplyr::c_across(nocare.cdf:nosay.cdf), na.rm = T)) |> 
  dplyr::mutate(exteff.cdf = dplyr::case_when(
    is.na(nocare) & is.na(nosay) ~ NA,
    .default = exteff.cdf
  )) |> 
  dplyr::ungroup() |>  
  # add variable label to each variable
  labelled::set_variable_labels(
    nocare.cdf = paste(attr(anes2024$nocare, "label"), "coded same as ANES CDF", sep = ", "),
    nosay.cdf = paste(attr(anes2024$nosay, "label"), "coded same as ANES CDF", sep = ", "),
    exteff.cdf = "External political efficacy index, unipolar scale"
  )


# create external efficacy index from recoded component items --------

# I aim to create an external political efficacy index that remains bipolar and
# symmetric around a neutral point. Negative values reflect less external
# efficacy (or rather, the opposing attitude in contrast), positive values
# reflect positive external efficacy, and the middling zero point reflects an
# insecure neutral position. This corresponds to the symmetrical response option
# format of the two external efficacy items in which respondents select one of
# five options to express the extent to which they agree, disagree, or whether
# they neither agree nor disagree with the given item statement.

# first, I recode the two items so that each ranges from -1 to 1. Specifically,
# "Agree strongly" = -1, "Agree somewhat" = -0.5, "Neither agree nor disagree" =
# 0, "Disagree somewhat" = 0.5, "Disagree strongly" = 1.
anes2024 <- anes2024 |>
  dplyr::mutate(dplyr::across(c(nocare, nosay), ~dplyr::case_when(
    .x == 1 ~ -1,
    .x == 2 ~ -0.5,
    .x == 3 ~ 0,
    .x == 4 ~ 0.5,
    .x == 5 ~ 1,
    TRUE ~ NA), .names = "{col}.recode")) |>
  dplyr::mutate(
    dplyr::across(
      c(nocare.recode, nosay.recode), 
      ~labelled::add_value_labels(., c("Agree strongly" = -1,
                                       "Agree somewhat" = -0.5,
                                       "Neither agree nor disagree" = 0,
                                       "Disagree somewhat" = 0.5,
                                       "Disagree strongly" = 1))))


# second, I create an external efficacy variable per respondent which is
# composed of the combination of responses to both external efficacy item
# responses. The responses are combined and summed to reflect a raw value/score
# of external efficacy on a 9-point range from -1 to 1.

anes2024 <- anes2024 |> 
  dplyr::rowwise() |> 
  # mean of sum = sum(x1, x2)/valid_n
  dplyr::mutate(exteff.indx = mean(c(nocare.recode, nosay.recode), na.rm = T)) |> 
  # dplyr::mutate(exteff.indx = sum(dplyr::c_across(nocare.recode:nosay.recode), na.rm = T)) |>
  dplyr::mutate(exteff.indx = dplyr::case_when(
    is.na(nocare) & is.na(nosay) ~ NA,
    .default = exteff.indx
  )) |> 
  dplyr::ungroup() |> 
  labelled::set_variable_labels(
    nocare.recode = attr(anes2024$nocare, "label"),
    nosay.recode = attr(anes2024$nosay, "label"),
    exteff.indx = "External political efficacy, bipolar scale"
  )


# create trust in government index ----------------------------------------

# trustgov1 PRE: How often trust government in Washington to do what is right [revised]
# 1 = [Always]             
# 2 = [Most of the time]   
# 3 = [About half the time]
# 4 = [Some of the time]   
# 5 = [Never]  

# trustgov2 Government run by a few big interests or for benefit of all
#  1 = [Run by a few big interests]       
#  2 = [For the benefit of all the people]

# trustgov3 Does government waste much tax money
# 1 = [Waste a lot]          
# 2 = [Waste some]           
# 3 = [Don't waste very much]

# trustgov4 How many in government are corrupt
# 1 = [All]       
# 2 = [Most]      
# 3 = [About half]
# 4 = [A few]     
# 5 = [None]

# create recoded trust in gov items following the ANES CDF coding scheme 
anes2024 <- anes2024 |> 
  dplyr::mutate(
    trustgov1.cdf = dplyr::case_when(
      trustgov1 %in% c(5, 4) ~ 0,
      trustgov1 == 3 ~ 33,
      trustgov1 == 2 ~ 67,
      trustgov1 == 1 ~ 100,
      .default = trustgov1),
    trustgov2.cdf = dplyr::case_when(
      trustgov2 == 1 ~ 0,
      trustgov2 == 2 ~ 100,
      .default = trustgov2),
    trustgov3.cdf = dplyr::case_when(
      trustgov3 == 1 ~ 0,
      trustgov3 == 2 ~ 50,
      trustgov3 == 3 ~ 100,
      .default = trustgov3),
    trustgov4.cdf = dplyr::case_when(
      trustgov4 %in% c(5, 4) ~ 100,
      trustgov4 == 3 ~ 50,
      trustgov4 %in% c(2, 1) ~ 0,
      .default = trustgov4)) |> 
  dplyr::mutate(trustgov.indx = rowMeans(
    dplyr::pick(trustgov1.cdf, trustgov2.cdf, trustgov3.cdf, trustgov4.cdf), na.rm = T)
    )

anes2024 <- anes2024 |> 
  # add variable label to each variable
  labelled::set_variable_labels(
    trustgov1.cdf = paste(attr(anes2024$trustgov1, "label"), "coded same as ANES CDF", sep = ", "),
    trustgov2.cdf = paste(attr(anes2024$trustgov2, "label"), "coded same as ANES CDF", sep = ", "),
    trustgov3.cdf = paste(attr(anes2024$trustgov3, "label"), "coded same as ANES CDF", sep = ", "),
    trustgov4.cdf = paste(attr(anes2024$trustgov4, "label"), "coded same as ANES CDF", sep = ", "),
    trustgov.indx = "Trust in Government Index (ANES CDF coded)"
  )


# make factor versions of select vars --------------------------------------

# convert select variables into factor, identify them with a suffix ".fct"
anes2024 <- anes2024 |>
  dplyr::mutate(dplyr::across(
    c(
      complete_status,
      intv_mode_post,
      sample_type,
      parents,
      birthplace,
      whereraised,
      residentyrs,
      drvlic,
      passport,
      othergovid,
      race,
      hisp,
      dplyr::contains("partyid"),
      educ,
      educ5,
      mil,
      sex,
      gender,
      income,
      income6,
      region,
      nocare,
      nosay,
      complex,
      understand,
      trustgov1,
      trustgov2,
      trustgov3,
      trustgov4,
      trustcrt,
      trustppl,
      trustelectnoff,
      govresp
    ),
    ~ sjlabelled::as_label(.),
    .names = "{col}.fct"
  ))


# check it out
# anes2024 |>
#   dplyr::select(dplyr::contains("fct")) |>
#   purrr::map(levels)

# shorten (recode) levels of certain factors
# first assign "new level" = "old level"
# Note that the old levels must match exactly
race_lvls <- c(
  "White"       = "White, non-Hispanic",
  "Black"       = "Black, non-Hispanic",                          
  "Hispanic"    = "Hispanic",                                                           
  "Asian/NH/PI" = "Asian or Native Hawaiian/other Pacific Islander, non-Hispanic",
  "Native American/ALSK Native" = "Native American/Alaska Native or other race, non-Hispanic",  
  "Other/multiple race" = "Multiple races, non-Hispanic")

educ5_lvls <- c(
  "Less than HS" = "Less than high school credential",
  "HS" = "High school credential",
  "Post HS" = "Some post-high school, no bachelor's degree",
  "Bachelor's" = "Bachelor's degree",
  "Graduate" = "Graduate degree"
)

gender_lvls <- c(
  "Man" = "Man",
  "Woman" = "Woman",
  "Nonbinary" = "Nonbinary",
  "Something else" = "Something else, please specify"
)

# recode levels of select factor variables
anes2024 <- anes2024 |> 
  dplyr::mutate(race.fct = forcats::fct_recode(race.fct, !!!race_lvls),
                educ5.fct = forcats::fct_recode(educ5.fct, !!!educ5_lvls),
                gender.fct = forcats::fct_recode(gender.fct, !!!gender_lvls))

# check
# anes2024 |>  
#   dplyr::select(educ5.fct, race.fct, gender.fct) |> 
#   purrr::map(levels)


# convert interview dates to proper date class objects
anes2024 <- anes2024 |>
  # replace certain empty strings with NA
  dplyr::mutate(dplyr::across(c(intv_sdate_pre, intv_sdate_post,
                                intv_edate_pre, intv_edate_post),
                              ~dplyr::na_if(x = ., y = "         ."))) |>
  # convert to year-month-day date format
  dplyr::mutate(dplyr::across(c(intv_sdate_pre, intv_sdate_post,
                                intv_edate_pre, intv_edate_post),
                              ~lubridate::ymd(.)))

# re-apply variable labels to date variables from raw dataset
anes2024 <- anes2024 |>
  labelled::set_variable_labels(
    intv_sdate_pre  = paste(attr(anes2024_raw$V243050, "label")),
    intv_sdate_post = paste(attr(anes2024_raw$V243052, "label")),
    intv_edate_pre  = paste(attr(anes2024_raw$V243060, "label")),
    intv_edate_post = paste(attr(anes2024_raw$V243062, "label"))
  )


# save ANES 2024 subset data and codebook ---------------------------------

# save data subset
readr::write_rds(anes2024, file = "data/anes data/anes2024_subset.rds")

# save codebook as .rds file
readr::write_rds(anes2024_codebook, file = "data/anes data/anes2024_subset_codebook.rds")



# clear environment -------------------------------------------------------

rm(list = ls())

# notes taken from the 2024 documentation ---------------------------------

# NOTE: 
# Two separate fresh cross-sectional samples were drawn, one for use with in-
# person interviewing as the primary mode and the second for web interviewing as
# the primary mode. In addition, the 2024 study features a panel of respondents
# previously completed the ANES in 2016 and 2020, as well as interviews with
# individuals who previously participated in the 2024 General Social Survey (GSS).

# Population. The two fresh samples for in-person and web interviews describe
# approximately the same population. The target population for the in-person
# sample was 232.5 million U.S. citizens age 18 or older living in the 48 contiguous
# states of the U.S. or Washington, D.C., and the target population for the web
# sample was 234.1 million U.S. citizens age 18 or older living in the 50 states or
# Washington, D.C. Both populations exclude those living in institutional or group
# quarters. In both modes, the sampling frame consisted of lists of residential
# addresses where mail is delivered. To be eligible to participate, a respondent had
# to reside at the sampled address and be a U.S. citizen aged 18 or older at the time
# of recruitment.

# NOTE: 
# Another component of the study involved a brief interview with the spouses or
# partners of respondents in the fresh sample groups who were in spousal or
# cohabiting relationships.

# In addition to the Questionnaire, ANES separately provides the PAPI (paper-and-
# pencil) and SPS (Spouse and Partner Survey) survey questionnaires in PDF format.
# Both documents are available for download from the 2024 Time Series study page
# of the ANES website.


# Analyses should be weighted to represent the U.S. adult citizen population
# accurately. Sampling error calculations should account for the complex sample
# design and the effects of weighting on variance.

# The data can be analyzed using the combined fresh sample, the fresh in-person
# sample alone, or the fresh web sample alone.

#  In general, if any post-election data are used for an
# estimate (alone or in combination with pre-election data), a post-election weight
# variable should be used, while analyses using solely pre-election data should use
# a pre-election weight.

# The study is not a simple random sample, so statistical procedures for complex
# sample designs must be used to obtain correct estimates of sampling errors and
# correct indications of statistical significance.

# A note on subpopulation analysis or analysis with missing data
# When conducting design-consistent variance estimation, cases should be
# selected by designating a subpopulation for the analysis procedure, not by
# dropping cases from the analysis or excluding missing data listwise. For some
# analyses the estimated standard errors from these two methods may be the same,
# or nearly so, but in other circumstances, failing to designate a subpopulation
# makes the standard error estimates incorrect. Also, in some cases, failing to
# designate a subpopulation may prevent the calculation of standard errors
# altogether by reducing the number of cases in a stratum to one.

# A note on weights and modes
# The weights are organized by sample group, which is distinct from the mode of
# interview. As detailed above in the Data Collection section, the fresh in-person
# sample includes interviews conducted in-person, by telephone, and over live two-
# way video. Weighting using the “in-person weight” selects the in-person sample,
# not only face-to-face interviews. Sample and mode can be identified (variables
# V240002a, V240002b and V240003), but within sample groups, the mode of
# interview was not randomly assigned, so analysts should be cautious about
# making mode comparisons or attributing differences among modes of interview to
# the interview mode itself; apparent mode differences may also be due to selection
# effects.
