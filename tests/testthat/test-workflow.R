context("workflow")

test_that("workflow stable", {
	library(magrittr)
	library(purrr)
	library(dplyr)

	coalitions = list(c("cdu"), c("cdu", "fdp"), c("cdu", "fdp", "gruene"), 
		c("spd"), c("spd", "linke"), c("spd", "linke", "gruene"))
	coalas <- coalitions:::paste_coalitions(coalitions)
	## scrape
	survey <- scrape_wahlrecht() %>% slice(1)
	expect_data_frame(survey, nrows=1, ncols=11)
	expect_equal(colnames(survey), c("datum", "start", "end", "cdu", "spd", 
		"gruene", "fdp", "linke", "afd", "sonstige", "befragte"))

	## collapse
	survey <- collapse_parties(survey)
	expect_data_frame(survey, nrows = 1, ncols=5)
	expect_equal(colnames(survey), c("datum", "start", "end", "befragte", "survey"))
	expect_data_frame(survey$survey[[1]], nrows=7, ncols=3)
	expect_equal(colnames(survey$survey[[1]]), c("party", "percent", "votes"))

	## add draws 
	survey %<>% mutate(draws = map(survey, draw_from_posterior, nsim=10, seed=123))
	expect_data_frame(survey, nrows = 1, ncols=6)
	expect_equal(
		colnames(survey), 
		c("datum", "start", "end", "befragte", "survey", "draws"))
	expect_data_frame(survey$draws[[1]], nrows=10, ncols=7)
	expect_equal(colnames(survey$draws[[1]]), survey$survey[[1]]$party)

	expect_warning(draw_from_posterior(survey$survey[[1]], nsim=10, correction=0.5))

	## add seats after redistribution
	survey %<>% mutate(seats = map2(draws, survey, get_seats, distrib.fun=sls))
	expect_data_frame(survey, nrows=1, ncols=7)
	expect_equal(
		colnames(survey),
		c("datum", "start", "end", "befragte", "survey", "draws", "seats"))
	expect_data_frame(survey$seats[[1]], nrows=60, ncols=3)
	expect_equal(colnames(survey$seats[[1]]), c("sim", "party", "seats"))

	## get majorities
	survey %<>% mutate(majorities = map(seats, have_majority))
	expect_data_frame(survey, nrows=1, ncols=8, any.missing=FALSE)
	expect_equal(
		colnames(survey),
		c("datum", "start", "end", "befragte", "survey", "draws", "seats", 
			"majorities"))
	expect_data_frame(survey$majorities[[1]], nrows=10, ncols=6, 
		types="logical")
	expect_equal(colnames(survey$majorities[[1]]), coalas)

	## get probabilities
	survey %<>% mutate(probabilities = map(majorities, calculate_probs, 
		coalitions=coalitions))

	expect_data_frame(survey, nrows=1, ncols=9, any.missing=FALSE)
	expect_equal(
		colnames(survey),
		c("datum", "start", "end", "befragte", "survey", "draws", "seats", 
			"majorities", "probabilities"))
	expect_data_frame(survey$probabilities[[1]], nrows=6, ncols=2, 
		types=c("character", "numeric"))
	expect_equal(colnames(survey$probabilities[[1]]), c("coalition", "probability"))


	## wrapper
	survey <- scrape_wahlrecht() %>% slice(1) %>% collapse_parties
	probs <- get_probs(survey, nsim=10)
	expect_data_frame(probs, nrows=1, ncols=1)
	expect_equal(colnames(probs), "probabilities")
	expect_data_frame(probs$probabilities[[1]], nrows=6, ncols=2)
	expect_equal(colnames(probs$probabilities[[1]]), c("coalition", "probability"))

})
