context("Test helper function that work on survey objects")

test_that("Obtaining meta information works", {
	meta <- get_meta(.survey_sample)
	expect_data_frame(meta, nrow=21, ncol=5)
	expect_equal(colnames(meta), c("pollster", "date", "start", "end", "respondents"))
})


test_that("Obtaining latest survey works", {
	latest <- get_latest(.survey_sample)
	expect_data_frame(latest, nrow=1, ncol=6)
	expect_equal(latest$date, as.Date("2017-09-02"))
})