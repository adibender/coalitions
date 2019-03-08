library(dplyr)
## A data frame containing names of pollsters and the respective adress at
# which survey information can be obtained.
.pollster_df <- tibble::tibble(
  pollster = c(
    "allensbach",
    "emnid",
    "forsa",
    "fgw",
    "gms",
    "infratest",
    "insa"),
  address = c(
    "https://www.wahlrecht.de/umfragen/allensbach.htm",
    "https://www.wahlrecht.de/umfragen/emnid.htm",
    "https://www.wahlrecht.de/umfragen/forsa.htm",
    "https://www.wahlrecht.de/umfragen/politbarometer.htm",
    "https://www.wahlrecht.de/umfragen/gms.htm",
    "https://www.wahlrecht.de/umfragen/dimap.htm",
    "https://www.wahlrecht.de/umfragen/insa.htm"))


## .div_mat is used when calculating seat distributions
div_vec <- 0.5:(598 + 0.5)
.div_mat <- t(matrix(1 / div_vec, nrow = length(div_vec), ncol = 20))


## a survey of samples used for tests and examples
.survey_sample <- surveys_sample <- get_surveys() %>%
  tidyr::unnest() %>%
  filter(date <= as.Date("2017-09-02") & date >= as.Date("2017-01-06")) %>%
  group_by(pollster) %>%
  slice(1:3) %>% tidyr::nest(-pollster, .key = "surveys")


## A data frame of terms in German and English used by prettify_strings
.trans_df <- tibble::tribble(
  ~ german           , ~ german_pretty                , ~ english         , ~english_pretty             ,
  "allensbach"       , "Allensbach"                   , "allensbach"      , "Allensbach"                ,
  "emnid"            , "Emnid"                        , "emnid"           , "Emnid"                     ,
  "forsa"            , "Forsa"                        , "forsa"           , "Forsa"                     ,
  "fgw"              , "Forsch'gr. Wahlen"            , "fgw"             , "FGW"                       ,
  "gms"              , "GMS"                          , "gms"             , "GMS"                       ,
  "infratest"        , "Infratest dimap"              , "infratest"       , "Infratest dimap"           ,
  "INSA"             , "INSA"                         , "insa"            , "INSA"                      ,
  "start"            , "Beginn"                       , "start"           , "start"                     ,
  "end"              , "Ende"                         , "end"             , "end"                       ,
  "survey"           , "Umfrage"                      , "survey"          , "survey"                    ,
  "befragte"         , "Befragte"                     , "respondents"     , "respondents"               ,
  "institute"        , "Institut"                     , "pollster"        , "pollster"                  ,
  "datum"            , "Datum"                        , "date"            , "date"                      ,
  "cdu"              , "Union"                        , "cdu"             , "Union"                     ,
  "spd"              , "SPD"                          , "spd"             , "SPD"                       ,
  "gruene"           , "Gr\u00fcne"                   , "greens"          , "Greens"                    ,
  "fdp"              , "FDP"                          , "fdp"             , "FDP"                       ,
  "linke"            , "Die Linke"                    , "left"            , "The Left"                  ,
  "piraten"          , "Piraten"                      , "pirates"         , "Pirates"                   ,
  "afd"              , "AfD"                          , "afd"             , "AfD"                       ,
  "sonstige"         , "Sonstige"                     , "others"          , "Others"                    ,
  "cdu_spd"          , "Union - SPD"                  , "cdu_spd"         , "Union - SPD"               ,
  "cdu_fdp"          , "Union - FDP"                  , "cdu_fdp"         , "Union - FDP"               ,
  "cdu_gruene"       , "Union - Gr\u00fcne"           , "cdu_greens"      , "Union - Greens"            ,
  "cdu_fdp_gruene"   , "Union - FDP - Gr\u00fcne"     , "cdu_fdp_greens"  , "Union - FDP - Greens"      ,
  "linke_spd"        , "SPD - Die Linke"              , "left_spd"        , "SPD - The Left"            ,
  "fdp_gruene_spd"   , "SPD - FDP - Gr\u00fcne"       , "fdp_greens_spd"  , "SPD - FDP - Greens"        ,
  "gruene_linke_spd" , "SPD - Die Linke - Gr\u00fcne" , "greens_left_spd" , "SPD - The Left - Greens")

party_labels_de <- c(
  "cdu"    = "Union",
  "spd"    = "SPD",
  "greens" = "Greens",
  "fdp"    = "FDP",
  "left"   = "Left",
  "afd"    = "AfD",
  "others" = "Others"
)

.party_cols_de <- party_colors_de <- c(
  "cdu"     = "black",
  "spd"     = "#E3000F",
  "greens"  = "#46962b",
  "fdp"     = "#eec900",
  "left"    = "#cd1076",
  "pirates" = "brown",
  "afd"     = "skyblue",
  "others"  = "grey")

.btw13 <- tibble::tibble(
  institute = rep("election_de", 7),
  datum     = rep(as.Date("2013-09-22"), 7),
  start     = rep(as.Date("2013-09-22"), 7),
  end       = rep(as.Date("2013-09-22"), 7),
  befragte  = rep(71.5, 7),
  party     = c("cdu", "spd", "left", "greens", "fdp", "afd", "others"),
  percent   = c(41.5, 25.7, 8.6, 8.4, 4.8, 4.7, 6.3))

devtools::use_data(
  .div_mat,
  .pollster_df,
  .survey_sample,
  .trans_df,
  .party_cols_de,
  .btw13,
  internal  = TRUE,
  overwrite = TRUE)

devtools::use_data(
  party_labels_de,
  party_colors_de,
  surveys_sample,
  overwrite = TRUE)
