context("Test pooling")
test_that("Pooling works as expected", {

	pooled <- .survey_sample %>%
		pool_surveys(last_date = max(tidyr::unnest(.survey_sample)$datum))

	expect_data_frame(pooled, nrow=7, ncol=8)
	expect_equal(sum(pooled$percent), 100)
	expect_equal(round(sum(pooled$votes)), 3055)

	pool <- pooled %>% tidyr::nest(party:votes, .key="survey")
	pool <- pool %>% get_probabilities(., nsim=10) %>% unnest()
	expect_data_frame(pool, nrow=6, ncol=4)
	expect_subset(colnames(pool), c("institute", "datum", "coalition", "probability"))

})