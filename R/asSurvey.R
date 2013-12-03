#' Creates basic survey table from votes in percent

#' This functions takes votes in percent (per party) obtained from a survey, 
#' and returns a table containing votes (in percent) and party names. Conducts  
#' sanity checks along the way, such as checking that percentages add up to 1.
#'
#' @param votes.in.perc Votes in percent each party received in the survey 
#'  of interest. Can be set to \code{NA}, if parties are specified that are not 
#'  mentioned in the specific survey (otherwise the \code{parties} argument has 
#'  to be modified).
#' @param samplesize Number of respondents in survey.
#' @param parties Vector of same length and in the same orderas \code{votes.in.perc}
#' @param epsilon The parameter \code{votes.in.perc} should add up to one. 
#' This parameter controls the maximal numerical divergence allowed.

#' @return A \code{data.frame} containing input and absolute number of votes 
#' in survey per party.
#' @keywords survey
#' @seealso \code{\link{redistribute}}
#' @export

as_survey <- function(

    votes.in.perc, 
    samplesize,
    parties = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "AfD", 
                "FW", "Sonstige"), 
    epsilon = 10e-6) {
    

    ## test for data validity
    if( length(votes.in.perc) != length(parties)) 
        stop("'votes.in.perc' and 'parties' arguments must be of same length")
    if( abs(sum(votes.in.perc, na.rm = TRUE) - 1) > epsilon )
        stop("'votes.in.perc' don't add up to 1")
    
    ## remove parties not respresented in sample, i.e. votes.in.perc = NA
    dat <- data.frame(party = parties, votes.in.perc = votes.in.perc)
    dat <- dat[!is.na(dat$votes.in.perc), ]
    dat$votes <- dat$votes.in.perc * samplesize
    
    ## relevel factor variable
    dat$party <- factor(dat$party, levels = parties)
    
    ## return 
    dat
    
}