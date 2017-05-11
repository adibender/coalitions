#' Coalition probabilities based on one survey
#' 
#' Given one row from a data frame containing results of surveys (one  survey 
#' per row), returns specified coalition probabilities
#' 
#' @inheritParams draw_from_posterior
#' @inheritParams get_coalition_probabilities
#' @param row A single row of a data frame. Should contain percentages of votes 
#' for individual parties (that add up to 100) and sample size of survey. 
#' @param ... Further arguments passed to \code{draw_from_posterior}
#' @import checkmate
cprob_row <- function(
	row, 
	nsim       = 10000L,
	coalitions = list(
		c("CDU"), 
		c("CDU", "FDP"), 
		c("CDU", "FDP", "GRUENE"), 
		c("SPD"), 
		c("SPD", "LINKE"), 
		c("SPD", "LINKE", "GRUENE")),
	superior.coalitions = list(
		c(""), 
		c("CDU"), 
		c("CDU", "FDP"), 
		c(""), 
		c("SPD"), 
		c("SPD", "LINKE")),
	majority = 300L,
	...) {

	ind.parties <- colnames(row) %in%
		c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "PIRATEN", "SONSTIGE")
	ind.na <- is.na(row[, ind.parties])
	perc   <- as.numeric(row[, ind.parties])[!ind.na]
	
	stopifnot(all.equal(sum(perc), 100))

	survey <- as_survey(perc/100, samplesize=row$BEFRAGTE,
		parties=colnames(row)[ind.parties])
	draws  <- draw_from_posterior(survey, nsim=nsim, ...)
	seats  <- get_seat_distribution(draws, survey)
	probs  <- get_coalition_probabilities(seats, coalitions=coalitions,
		superior.coalitions=superior.coalitions, majority=majority)
	names(probs) <- paste0("Prob_", names(probs))
	
	cbind.data.frame(row[, !ind.parties], as.list(probs*100))

}


#' Coalition probabilities based on survey results
#' 
#' Given  data frame containing results from different surveys (one  survey 
#' per row), returns the provided data frame plus specified coalition probabilities.
#' @param survey.df A data frame containing survey results (one survey per row).
#' @inheritParams cprob_row
#' @param mc.cores Number of cores used for calculations.
#' @import checkmate parallel magrittr dplyr
#' @export
cprob_tab <- function(
	survey.df, 
	nsim       = 10000L,
	coalitions = list(
		c("CDU"), 
		c("CDU", "FDP"), 
		c("CDU", "FDP", "GRUENE"), 
		c("SPD"), 
		c("SPD", "LINKE"), 
		c("SPD", "LINKE", "GRUENE")),
	superior.coalitions = list(
		c(""), 
		c("CDU"), 
		c("CDU", "FDP"), 
		c(""), 
		c("SPD"), 
		c("SPD", "LINKE")),
	mc.cores=10L,
	...) {

	assert_data_frame(survey.df, all.missing=FALSE, min.cols=3, min.rows=1)
	assert_number(nsim, lower=1, na.ok=FALSE, finite=TRUE)
	assert_list(coalitions, types="character", any.missing=FALSE, min.len=1, 
		unique=TRUE)
	assert_list(superior.coalitions, types="character", any.missing=FALSE, 
		min.len=1)

	survey.df %<>% mutate(id=row_number())
	cprobs <- mclapply(
		split(survey.df, f=survey.df$id), 
		cprob_row, nsim=nsim, coalitions=coalitions, 
			superior.coalitions=superior.coalitions, 
		mc.cores=mc.cores)

	probs <- left_join(survey.df, bind_rows(cprobs))
	colnames(probs) <- gsub("-", "_", colnames(probs), fixed=TRUE)

	return(probs)

}
