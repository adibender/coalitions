#' Calculate percentage of votes/seats after excluding parties with 
#' \code{votes < 0.05}

#' @param survey table containing votes and votes in percent (as created e.g. by 
#' \code{\link{as_survey}})

#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.05 (hurdle for german parliament)

#' @keywords survey, hurdle

#' @seealso \code{\link{as_survey}}, \code{\link{sls}}

#' @export


redistribute <- function(survey, hurdle = 0.05) {

    survey <- survey[survey$votes.in.perc >= hurdle & survey$party != "Others", ]
    survey$votes.in.perc <- survey$votes/sum(survey$votes)
    
    survey
    
}
