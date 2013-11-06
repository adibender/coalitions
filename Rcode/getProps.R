#' Calculate percentage of votes/seats after excluding parties with 
#' \code{votes.in.perc < 0.05}

#' @param survey table containing votes and votes.in.perc (as created e.g. by 
#' \code{\link{createTab}})
#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.05 (hurdle for german parliament)

#' @keywords survey, hurdle

#' @seealso \code{\link{createTab}}, \code{\link{sls}}

#' @export

#' @examples

#' @examples
#' test.tab <- createTab( party = c(LETTERS[1:9])
#'        votes.in.perc = c(40.9, 25.4, 14.3, 4.7, 7.5, 1.9, 1.9, 1.3, 2.1)/100, 
#'        sample.size = 1006)
#' test.tab
#' get.props(test.tab)

get.props <- function(survey, hurdle = 0.05) {

    survey <- survey[survey$votes.in.perc >= hurdle & survey$party != "Sonstige", ]
    survey$votes.in.perc <- survey$votes/sum(survey$votes)
    
    survey
    
}
