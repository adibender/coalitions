# TODO: Function name changed (consider in functions that depend on this one)
drawElectionsFromPosterior <- function(survey, simulations, seed = NULL, prior = NULL) {
    
    ## for rdirichlet
    require(MCMCpack)
    # simulieren von anteilen
    # rdirichlet ruft rgamma auf mit rate = 1    
    
    ## calculate posteriori
    if( is.null(prior) ) {
        prior <- rep(0.5, nrow(survey))    
    }
    
    else {
        if( length(prior) != nrow(survey) ) 
            stop("length of prior weights and number of observations differ")
    }
    
    alpha <- survey$votes + prior
    
    ## draw n.sim random dirichlet numbers/vectors with concentration weights alpha
    if( !is.null(seed) ) set.seed(seed)
    rn <- as.data.frame(rdirichlet(simulations, alpha = alpha))
    colnames(rn) <- survey$party  
    
    rn
    
}