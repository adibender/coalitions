context("scrappers")

test_that("All scrapers work", {
hp.vec <- c(
	"allensbach"     = "http://www.wahlrecht.de/umfragen/allensbach.htm",
	"emnid"          = "http://www.wahlrecht.de/umfragen/emnid.htm",
	"forsa"          = "http://www.wahlrecht.de/umfragen/forsa.htm",
	"forschGrWahlen" = "http://www.wahlrecht.de/umfragen/politbarometer.htm",
	"gms"            = "http://www.wahlrecht.de/umfragen/gms.htm",
	"infratest"      = "http://www.wahlrecht.de/umfragen/dimap.htm", 
	"insa"           =  "http://www.wahlrecht.de/umfragen/insa.htm")

expect_data_frame(head(scrape_wahlrecht(hp.vec[1])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[2])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[3])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[4])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[5])), ncols=13, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[6])), ncols=11, nrows=6)

})


context("Transform to long format") 
test_that("Collapse parties works correctly", {
	surveys <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/gms.htm")[1,]
	surveys[, 5] <- NA	
	expect_data_frame(cp <- collapse_parties(surveys), nrows=1, ncols=5)
})

context("No missing values in percent columns")
test_that("GMS tables correct", {
	
	gms <- get_surveys() %>%
		filter(institute == "gms") %>%
		unnest() %>%
		filter(datum == "2017-06-01") %>%
		unnest()
	expect_identical(sum(gms$percent), 100)
	expect_false(any(is.na(gms$percent)))

})

