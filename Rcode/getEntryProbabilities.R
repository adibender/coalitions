getEntryProbabilities <- function(dirichlet.draws) {
    
    ## dirichlet.draws: matrix off random election results as returned by 
    # drawDirichletElections
    colSums(dirichlet.draws >= 0.05)/nrow(dirichlet.draws)
    
}

getLentEntryProbs <- function(dirichlet.draws, max.percent.lent = 10) {
    
    lent.vec <- 0:max.percent.lent / 100
    lent.drn <- lapply(lent.vec, lentVotesRn, rn.mat = dirichlet.draws)
    
    entry.probs <- sapply(lent.drn, getProbabilitiesEntry)
    colnames(entry.probs) <- lent.vec
    
    entry.probs
    
}