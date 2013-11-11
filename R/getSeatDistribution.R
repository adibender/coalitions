getSeatDistribution <- function(dirichlet.draws, survey, distrib.fun = sls, 
        sample.size = NULL, ...) {
    
    if( is.null(sample.size) ) sample.size <- sum(survey$votes)
    sim.surveys <- lapply(seq_len(nrow(dirichlet.draws)), function(z) {
                
                survey$votes.in.perc <- as.numeric(dirichlet.draws[z, ])
                survey$votes <- survey$votes.in.perc * sample.size
                survey
                
            })
    
    ## calculate seat distribution for each simulation via sls() function 
    sim.results <- lapply(sim.surveys, distrib.fun, ...)
    
    ## return results
    sim.results
    
}