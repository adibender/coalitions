#' Get probabilities to enter the parliament. 
#' 
#' @param dirichlet.draws Matrix or data frame containing draws from 
#' the posterior (see \code{\link{draw_from_posterior}}).
#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.05 (hurdle for german parliament).
#' @return Vector of (named) entry probabilities.
#' @export
#' @seealso \code{\link{draw_from_posterior}}
#' 
get_entryprobability <- function(dirichlet.draws, hurdle = 0.05) {
    
  colSums(dirichlet.draws >= hurdle)/nrow(dirichlet.draws)
    
}


# get_lent_entryprobabilities <- function(dirichlet.draws, hurdle = 0.05, 
# 	max.percent.lent = 10, from, to) {
    
#     lent.vec <- 0:max.percent.lent / 100
#     lent.drn <- lapply(lent.vec, lentVotesRn, rn.mat = dirichlet.draws, 
#     	from = from, to = to)
    
#     entry.probs <- sapply(lent.drn, getProbabilitiesEntry, hurdle = hurdle)
#     colnames(entry.probs) <- lent.vec
    
#     entry.probs
    
# }