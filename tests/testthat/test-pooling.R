context("Test pooling")
test_that("Pooling works as expected", {

  pooled <- .survey_sample %>%
    pool_surveys(last_date = max(tidyr::unnest(.survey_sample)$date))

  expect_data_frame(pooled, nrow=7, ncol=8)
  expect_equal(sum(pooled$percent), 100)
  expect_equal(round(sum(pooled$votes)), 3055)

  pool <- pooled %>% tidyr::nest(party:votes, .key="survey")
  pool <- pool %>% get_probabilities(., nsim=10) %>% unnest()
  expect_data_frame(pool, nrow=6, ncol=4)
  expect_subset(colnames(pool), c("pollster", "date", "coalition",
    "probability"))

  ## test that duplicated percentages don't break pool_surveys
  s1          <- .survey_sample %>% unnest() %>% slice(1) %>% unnest()
  s1$percent  <- c(34, 34, 8.5, 8, 4.5, 8, 3)
  s1$votes    <- s1$percent * 1421/100
  s2          <- s1
  s2$pollster <- "emnid"
  s2$date     <- as.Date("2016-08-22")
  s12         <- bind_rows(s1, s2) %>%
    nest(party:votes, .key="survey") %>%
    nest(-pollster, .key="surveys")
  pool <- pool_surveys(s12, last_date=as.Date("2017-08-22"))
  expect_data_frame(pool, nrow=7, ncol=8)

})
