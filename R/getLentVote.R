## lent.votes posteriori, that is after drawing random numbers with original 
## votes, i.e. for each vector of random numbers, redistribute
## votes from CDU/CSU to FDP 
lentVotesRn <- function(rn.mat, lent, from = "CDU/CSU", to = "FDP") {
    
    if( lent != 0) {
        lent.pp <- rn.mat[, from] * lent
        rn.mat[, from] <- rn.mat[, from] -lent.pp
        rn.mat[, to] <- rn.mat[, to] + lent.pp
    }
    rn.mat
    
}