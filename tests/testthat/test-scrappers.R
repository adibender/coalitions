context("scrappers")

test_that("All scrapers work", {
hp.vec <- c(
	"allensbach"     = "http://www.wahlrecht.de/umfragen/allensbach.htm",
	"emnid"          = "http://www.wahlrecht.de/umfragen/emnid.htm",
	"forsa"          = "http://www.wahlrecht.de/umfragen/forsa.htm",
	"forschGrWahlen" = "http://www.wahlrecht.de/umfragen/politbarometer.htm",
	"gms"            = "http://www.wahlrecht.de/umfragen/gms.htm",
	"infratest"      = "http://www.wahlrecht.de/umfragen/dimap.htm",
	"insa"           = "http://www.wahlrecht.de/umfragen/insa.htm")

expect_data_frame(head(scrape_wahlrecht(hp.vec[1])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[2])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[3])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[4])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[5])), ncols=13, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[6])), ncols=11, nrows=6)

survey <- scrape_wahlrecht(address = "http://www.wahlrecht.de/umfragen/insa.htm" ) %>%
	filter(datum==as.Date("2017-08-01"))
expect_data_frame(survey, nrows=1, ncols=13)
expect_equal(colnames(survey), c("datum", "start", "end", "cdu", "spd",
	"gruene", "fdp", "linke", "piraten", "fw", "afd", "sonstige", "befragte"))
expect_equal(survey$spd, 24.5)
expect_equal(survey$befragte, 2046)

survey2 <- scrape_wahlrecht(address = "http://www.wahlrecht.de/umfragen/allensbach.htm") %>%
	filter(datum==as.Date("2017-07-18"))
expect_data_frame(survey2, nrows=1, ncols=11)
expect_equal(survey2$cdu, 39.5)
expect_equal(survey2$befragte, 1403)

})


context("Transform to long format")
test_that("Collapse parties works correctly", {
	surveys <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/gms.htm")[1,]
	surveys[, 5] <- NA
	expect_data_frame(cp <- collapse_parties(surveys), nrows=1, ncols=5)
})

context("No missing values in percent columns")
test_that("GMS tables correct", {

	gms <- .survey_sample %>%
		filter(institute == "gms") %>%
		unnest() %>%
		filter(datum == "2017-06-01") %>%
		unnest()
	expect_identical(sum(gms$percent), 100)
	expect_false(any(is.na(gms$percent)))

})

