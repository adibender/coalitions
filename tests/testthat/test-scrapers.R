context("scrapers")

test_that("State wide german scrapers work", {
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
	filter(date==as.Date("2017-08-01"))
expect_data_frame(survey, nrows=1, ncols=13)
expect_equal(colnames(survey), c("date", "start", "end", "cdu", "spd",
	"greens", "fdp", "left", "pirates", "fw", "afd", "others", "respondents"))
expect_equal(survey$spd, 24.5)
expect_equal(survey$respondents, 2046)

survey2 <- scrape_wahlrecht(address = "http://www.wahlrecht.de/umfragen/allensbach.htm") %>%
	filter(date==as.Date("2017-07-18"))
expect_data_frame(survey2, nrows=1, ncols=11)
expect_equal(survey2$cdu, 39.5)
expect_equal(survey2$respondents, 1403)

})

test_that("Federal german scrapers work", {
	# Bayern
	by <- scrape_by() %>% filter(date == "2018-02-12")
	expect_data_frame(by, nrows=1, ncols=13)
	expect_equal(by$csu, 40)
	expect_equal(by$respondents, 1510)

	surveys_by <- get_surveys_by()
	expect_data_frame(surveys_by, nrows = 6, ncols = 2)

	# Niedersachsen
	nds <- scrape_ltw() %>% filter(date == "2017-10-12")
	expect_data_frame(nds, nrows=1, ncols=12)
	expect_equal(nds$spd, 34.5)
	expect_equal(nds$respondents, 1001)

	surveys_nds <- get_surveys_nds()
	expect_data_frame(surveys_nds, nrows = 6, ncols = 2)

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
		filter(pollster == "gms") %>%
		unnest() %>%
		filter(date == "2017-06-01") %>%
		unnest()

	expect_identical(sum(gms$percent), 100)
	expect_false(any(is.na(gms$percent)))

})
