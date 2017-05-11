context("Survey object")
test_that("as_survey works correctly", {
	expect_data_frame(
		forsa <- as_survey(
			percent    = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 0.04),
			samplesize = 2508,
			parties    = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "sonstige")),
		nrows=8L, ncols=3L)
	expect_subset(colnames(forsa), c("party", "percent", "votes"))
	expect_equal(forsa$votes, 
		c(1028.28, 601.92, 326.04, 100.32, 200.64, 75.24, 75.24, 100.32))
})


context("Redistribution")
test_that("Redistritubion works correctly", {
	forsa <- as_survey(
		percent    = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 0.04),
		samplesize = 2508,
		parties    = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "sonstige"))
	expect_data_frame(result <- redistribute(forsa, hurdle = 0.05), 
		nrows=4, ncols=3, any.missing = FALSE)
	expect_equal(round(result$percent, 2), c(0.48, 0.28, 0.15, 0.09))
})


context("Sainte-Lague/Scheppers")
test_that("Sainte-Lague/Scheppers works correctly", {
	forsa <- as_survey(
		percent    = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 0.04),
		samplesize = 2508,
		parties    = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "sonstige"))
	expect_data_frame(seats <- sls(redistribute(forsa), seats = 598), 
		nrows=4, ncols=2)
	expect_subset(colnames(seats), c("party", "seats"))
	expect_identical(seats$seats, c(285L, 167L, 90L, 56L))

})


context("Draw from posterior")
test_that("Sainte-Lague/Scheppers works correctly", {
	forsa <- as_survey(
		percent    = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 0.04),
		samplesize = 2508,
		parties    = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "sonstige"))
	draws <- draw_from_posterior(forsa, nsim=10)
	expect_data_frame(draws, nrow=10, ncol=8)
	expect_error(draw_from_posterior(forsa, nsim=10, prior=c(0.5, 0.5, 0.5, 0.5)))
	
})
