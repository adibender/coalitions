context("Survey object")
test_that("as_survey works correctly", {
	expect_is(
		forsa <- as_survey(
			votes.in.perc = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 0.04), 
			samplesize    = 2508, 
			parties       = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", 
				"AfD", "Others")), "data.frame")
	expect_equal(nrow(forsa), 8L)
	expect_equal(ncol(forsa), 3L)
	expect_equal(forsa$votes, 
		c(1028.28, 601.92, 326.04, 100.32, 200.64, 75.24, 75.24, 100.32))
})


context("Redistribution")
test_that("Redistritubion works correctly", {
	expect_is(result <- redistribute(forsa, hurdle = 0.05), "data.frame")
	expect_equal(nrow(result), 4)
	expect_equal(ncol(result), 3)
	expect_equal(round(result$votes.in.perc, 2), c(0.48, 0.28, 0.15, 0.09))
})


context("Sainte-Lague/Scheppers")
test_that("Sainte-Lague/Scheppers works correctly", {
	expect_is(seats <- sls(redistribute(forsa), seats = 598), "data.frame")
	expect_equal(nrow(seats), 4)
	expect_equal(ncol(seats), 4)
	expect_identical(seats$seats, c(285L, 90L, 56L, 167L))
})
