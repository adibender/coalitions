#' Calculate percentage of votes/seats after excluding parties with 
#' \code{votes < hurdle}
#' 
#' @param survey table containing votes and votes in percent (as created e.g. by 
#' \code{\link{as_survey}})
#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.05 (hurdle for german parliament)
#' @param others Name under which parties far below the \code{hurdle} are 
#' summarized. 
#' @param epsilon Percentages should add up to 1. If they do not, within accuracy
#' of \code{epsilon}, an error is thrown.
#' @keywords survey, hurdle
#' @seealso \code{\link{as_survey}}, \code{\link{sls}}
#' @export
redistribute <- function(
	survey,
	hurdle  = 0.05,
	others  = "sonstige",
	epsilon = 10e-6) {

	## must be & (we include parties with percent > hurdel and name != others)
	survey <- survey[survey$percent >= hurdle & survey$party != others, ]

	survey$percent <- survey$votes/sum(survey$votes)

	# check for data validity
  if (abs(sum(survey$percent) - 1) > epsilon) 
    stop("wrong percentages calculated in 'redistribute()' function")
	
	survey
	
}
