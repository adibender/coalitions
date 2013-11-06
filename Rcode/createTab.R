#' creates table containing votes (in percent) and party names, conducts sanity 
#' checks

#' @param votes.in.perc votes in percent each party received in the survey 
#' of interest. Can be set to \code{NA}, if parties are specified that are not 
#' mentioned in the specific survey (otherwise the \code{parties} argument has 
#' to be modified)
#' @param sample.size number of respondents in survey
#' @parties vector of same length as 'votes.in.perc' and in the same order
#' as \code{votes.in.perc}

createTab <- function(
        votes.in.perc, sample.size,
        parties = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "AfD", 
                "FW", "Sonstige"), epsilon = 10e-6) {
    

    
    ## test for data validity
    if( length(votes.in.perc) != length(parties)) 
        stop("'votes.in.perc' and 'parties' arguments must be of same length")
    if( abs(sum(votes.in.perc, na.rm = TRUE) - 1) > epsilon )
        stop("'votes.in.perc' don't add up to 1")
    
    ## remove parties not respresented in sample, i.e. votes.in.perc = NA
    dat <- data.frame(party = parties, votes.in.perc = votes.in.perc)
    dat <- dat[!is.na(dat$votes.in.perc), ]
    dat$votes <- dat$votes.in.perc * sample.size
    
    ## relevel factor variable
    dat$party <- factor(dat$party, levels = parties)
    
    ## return 
    dat
    
}