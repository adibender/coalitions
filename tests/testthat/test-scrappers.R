context("scrappers")

test_that("All scrapers work", {
hp.vec <- c(
	"allensbach"     = "http://www.wahlrecht.de/umfragen/allensbach.htm",
	"emnid"          = "http://www.wahlrecht.de/umfragen/emnid.htm",
	"forsa"          = "http://www.wahlrecht.de/umfragen/forsa.htm",
	"forschGrWahlen" = "http://www.wahlrecht.de/umfragen/politbarometer.htm",
	"gms"            = "http://www.wahlrecht.de/umfragen/gms.htm",
	"infratest"      = "http://www.wahlrecht.de/umfragen/dimap.htm")

cnames <- c("datum", "start", "end", "cdu", "spd", "gruene", "fdp", "linke", 
	"afd", "sonstige")

expect_data_frame(head(scrape_wahlrecht(hp.vec[1])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[2])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[3])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[4])), ncols=11, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[5])), ncols=13, nrows=6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[6])), ncols=11, nrows=6)

})
