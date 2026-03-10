context("scrapers")

test_that("State wide german scrapers work", {

  skip_if_offline()

hp.vec <- c(
  "allensbach"     = "https://www.wahlrecht.de/umfragen/allensbach.htm",
  "emnid"          = "https://www.wahlrecht.de/umfragen/emnid.htm",
  "forsa"          = "https://www.wahlrecht.de/umfragen/forsa.htm",
  "forschGrWahlen" = "https://www.wahlrecht.de/umfragen/politbarometer.htm",
  "gms"            = "https://www.wahlrecht.de/umfragen/gms.htm",
  "infratest"      = "https://www.wahlrecht.de/umfragen/dimap.htm",
  "insa"           = "https://www.wahlrecht.de/umfragen/insa.htm")

expect_data_frame(head(scrape_wahlrecht(hp.vec[1])), min.cols = 11, nrows = 6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[2])), min.cols = 11, nrows = 6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[3])), min.cols = 11, nrows = 6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[4])), min.cols = 11, nrows = 6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[5])), min.cols = 11, nrows = 6)
expect_data_frame(head(scrape_wahlrecht(hp.vec[6])), min.cols = 11, nrows = 6)

survey <- scrape_wahlrecht(
  address = "https://www.wahlrecht.de/umfragen/insa.htm" ) %>%
  filter(date == as.Date("2024-01-22"))
expect_data_frame(survey, nrows = 1, min.cols = 11)
expect_true(all(c("date", "start", "end", "cdu", "spd",
  "greens", "fdp", "left", "afd", "others", "respondents") %in% colnames(survey)))
expect_equal(survey$spd, 13.5)
expect_equal(survey$respondents, 2006)

survey2 <- scrape_wahlrecht(
    address = "https://www.wahlrecht.de/umfragen/allensbach.htm") %>%
  filter(date == as.Date("2021-05-19"))
expect_data_frame(survey2, nrows = 1, min.cols = 11)
expect_equal(survey2$cdu, 27.5)
expect_equal(survey2$respondents, 1027)

})

test_that("Federal german scrapers work", {

  skip_if_offline()

  # Bayern
  by <- scrape_by() %>% filter(date == as.Date("2018-02-12"))
  expect_data_frame(by, nrows = 1, min.cols = 12)
  expect_equal(by$csu, 40)
  expect_equal(by$respondents, 1510)

  surveys_by <- get_surveys_by()
  expect_data_frame(surveys_by, nrows = 7, ncols = 2)

  # Niedersachsen
  nds <- scrape_ltw() %>% filter(date == as.Date("2017-10-12"))
  expect_data_frame(nds, nrows = 1, min.cols = 11)
  expect_equal(nds$spd, 34.5)
  expect_equal(nds$respondents, 1001)

  surveys_nds <- get_surveys_nds()
  expect_data_frame(surveys_nds, nrows = 7, ncols = 2)

  # Hessen
  he <- scrape_ltw(
    address = "https://www.wahlrecht.de/umfragen/landtage/hessen.htm",
    ind_row_remove = -1) %>%
    filter(date <= as.Date("2018-09-07"))
  expect_data_frame(he, min.rows = 10, min.cols = 11)
  expect_true("respondents" %in% colnames(he))
  expect_true("others" %in% colnames(he))

  # Rheinland-Pfalz
  rp <- scrape_rp() %>%
    filter(date <= as.Date("2020-12-31") & date >= as.Date("2020-01-01"))
  expect_data_frame(rp, min.rows = 1, min.cols = 12)
  expect_identical(rp$respondents[1], 1002)
  expect_identical(rp$others[rp$date == as.Date("2020-04-15")], 5)

  surveys_rp <- get_surveys_rp()
  expect_data_frame(surveys_rp, nrows = 5, ncols = 2)
})


context("Transform to long format")
test_that("Collapse parties works correctly", {

  skip_if_offline()

  surveys <- scrape_wahlrecht("https://www.wahlrecht.de/umfragen/gms.htm")[1,]
  surveys[, 5] <- NA
  expect_data_frame(cp <- collapse_parties(surveys), nrows = 1, ncols = 5)
})

context("No missing values in percent columns")
test_that("Survey sample tables correct", {

  emnid <- .survey_sample %>%
    filter(pollster == "emnid") %>%
    unnest("surveys") %>%
    filter(date == as.Date("2017-09-02")) %>%
    unnest("survey")

  expect_identical(sum(emnid$percent), 100)
  expect_false(any(is.na(emnid$percent)))

})
