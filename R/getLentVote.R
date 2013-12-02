#' Votes lent from one party to another for strategic reasons but not revealed 
#' in survey.


#' @param rn.mat Matrix containing draws from posterior 
#' see \code{\link{drawElectionsFromPosterior}}
#' @param lent Percentage of votes lent from one party to another
#' @param from Party from which votes are lent
#' @param to Party to which votes are lent

#' @return \code{rn.mat} adjusted for lent.votes

#' @seealso \code{\link{drawElectionsFromPosterior}}



lent_votes_rn <- function(rn.mat, lent, from = "CDU/CSU", to = "FDP") {
    
    if( lent != 0) {
        lent.pp <- rn.mat[, from] * lent
        rn.mat[, from] <- rn.mat[, from] -lent.pp
        rn.mat[, to] <- rn.mat[, to] + lent.pp
    }
    
    rn.mat
    
}