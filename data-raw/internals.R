library(dplyr)
## A data frame containing names of pollsters and the respective adress at
# which survey information can be obtained.
.institutes_df <- tibble::tibble(
	institute = c(
		"allensbach",
		"emnid",
		"forsa",
		"fgw",
		"gms",
		"infratest",
		"insa"),
	address = c(
		"http://www.wahlrecht.de/umfragen/allensbach.htm",
		"http://www.wahlrecht.de/umfragen/emnid.htm",
		"http://www.wahlrecht.de/umfragen/forsa.htm",
		"http://www.wahlrecht.de/umfragen/politbarometer.htm",
		"http://www.wahlrecht.de/umfragen/gms.htm",
		"http://www.wahlrecht.de/umfragen/dimap.htm",
		"http://www.wahlrecht.de/umfragen/insa.htm"))


## .div_mat is used when calculating seat distributions
div_vec <- 0.5:(598+0.5)
.div_mat <- t(matrix(1/div_vec, nrow=length(div_vec), ncol=20))


## a survey of samples used for tests and examples
.survey_sample <- get_surveys() %>%
	tidyr::unnest() %>%
	filter(datum <= as.Date("2017-09-02") & datum >= as.Date("2017-01-06")) %>%
	group_by(institute) %>%
	slice(1:3)


## A data frame of terms in German and English used by prettify_strings
.trans_df <- tibble::tribble(
	~ german           , ~ german_pretty                , ~ english         , ~english_pretty             ,
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
	"cdu_fdp"          , "Union - FDP"                  , "cdu_fdp"         , "Union - FDP"               ,
	"cdu_gruene"       , "Union - Gr\u00fcne"           , "cdu_greens"      , "Union - Greens"            ,
	"cdu_fdp_gruene"   , "Union - FDP - Gr\u00fcne"     , "cdu_fdp_greens"  , "Union - FDP - Greens"      ,
	"linke_spd"        , "SPD - Die Linke"              , "left_spd"        , "SPD - The Left"            ,
	"gruene_linke_spd" , "SPD - Die Linke - Gr\u00fcne" , "greens_left_spd" , "SPD - The Left - Greeens")

devtools::use_data(
	.div_mat,
	.institutes_df,
	.survey_sample,
	.trans_df,
	internal  = TRUE,
	overwrite = TRUE)