#' Calculate seat distribution from draws from posterior

#' @param dirichlet.draws Matrix containing random draws from posterior.
#' @param survey The actual survey results on which \code{dirichlet.draws}
#' were based on.
#' @param distrib.fun Function to calculate seat distribution. Defaults to 
#' \code{\link{sls}} (Sainte-Lague/Scheppers). 
#' @param sample.size Number of individuals participating in the \code{survey}.
#' @param ... Further arguments passed to \code{distrib.fun}.

#' @return \code{list} containing seat distributions for each simulation in 
#' \code{dirichlet.draws}

#' @keywords seat distribution

#' @seealso \code{\link{drawElectionsFromPosterior}}, \code{\link{sls}}, 
#' \code{\link{dHondt}}

#' @export



getSeatDistribution <- function(dirichlet.draws, survey, distrib.fun = sls, 
        sample.size = NULL, ...) {
    
    if( is.null(sample.size) ) sample.size <- sum(survey$votes)
    sim.surveys <- lapply(seq_len(nrow(dirichlet.draws)), function(z) {
                
                survey$votes.in.perc <- as.numeric(dirichlet.draws[z, ])
                survey$votes <- survey$votes.in.perc * sample.size
                survey
                
            })
    
    ## calculate seat distribution for each simulation
    sim.results <- lapply(sim.surveys, distrib.fun, ...)
    
    ## return results
    sim.results
    
}