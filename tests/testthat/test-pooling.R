context("Test pooling")
test_that("Pooling works as expected", {

  pooled <- .survey_sample %>%
    pool_surveys(last_date = max(tidyr::unnest(.survey_sample, "surveys")$date))

  expect_data_frame(pooled, nrow=7, ncol=8)
  expect_equal(sum(pooled$percent), 100)

  pool <- pooled %>% tidyr::nest(survey = party:votes)
  pool <- pool %>% get_probabilities(., nsim=10) %>% unnest(probabilities)
  expect_data_frame(pool, nrow=6, ncol=4)
  expect_subset(colnames(pool), c("pollster", "date", "coalition",
    "probability"))

  ## test that duplicated percentages don't break pool_surveys
  s1 <- .survey_sample %>% ungroup() %>% unnest(surveys) %>% slice(1)
  s2 <- s1
  s2$pollster <- "other_pollster"
  s2$date     <- s2$date - 1
  s12 <- bind_rows(s1, s2) %>%
    nest(surveys = -pollster)
  pool <- pool_surveys(s12, last_date = s1$date)
  expect_data_frame(pool, nrow=7, ncol=8)

})
