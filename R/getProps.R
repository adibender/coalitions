#' Calculate percentage of votes/seats after excluding parties with 
#' \code{votes.in.perc < 0.05}

#' @param survey table containing votes and votes.in.perc (as created e.g. by 
#' \code{\link{createTab}})

#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.05 (hurdle for german parliament)

#' @keywords survey, hurdle

#' @seealso \code{\link{createTab}}, \code{\link{sls}}

get.props <- function(survey, hurdle = 0.05) {

    survey <- survey[survey$votes.in.perc >= hurdle & survey$party != "Sonstige", ]
    survey$votes.in.perc <- survey$votes/sum(survey$votes)
    
    survey
    
}
