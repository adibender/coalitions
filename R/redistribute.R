#' Calculate percentage of votes/seats after excluding parties with 
#' \code{votes < hurdle}
#' 
#' @param survey table containing votes and votes in percent (as created e.g. by 
#' \code{\link{as_survey}})
#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.05 (hurdle for german parliament)
#' @export
#' @keywords survey, hurdle
#' @seealso \code{\link{as_survey}}, \code{\link{sls}}

redistribute <- function(survey, hurdle = 0.05) {

	survey <- survey[survey$votes.in.perc >= hurdle & survey$party != "Others", ]
	survey$votes.in.perc <- survey$votes/sum(survey$votes)
	
	survey
	
}

#' @rdname redistribute
#' @inheritParams redistribute
redistribute2 <- function(survey, hurdle = 0.05) {

	survey <- survey[survey$PERCENT >= hurdle & survey$PARTY != "SONSTIGE", ]
	survey$PERCENT <- survey$VOTES/sum(survey$VOTES)
	
	survey
	
}
