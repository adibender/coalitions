# TODO: Function name changed (consider in functions that depend on this one)

#' Draw random numbers from posterior distribution
#' 
#' @param survey survey object as returned by \code{createTab} or \code{getSurveys}
#' @param nsim number of simulations
#' @param seed sets seed
#' @param prior optional prior information. Defaults to 1/2 (Jeffrey's prior). 

#' @return \code{data.frame} containing random draws from dirichlet distribution
#' which can be interpreted as election results.
#' @keywords draw, simulate
#' @seealso \code{\link{createTab}} \code{\link{getSurveys}}
#' @export
#' @examples 
#' 


drawElectionsFromPosterior <- function(survey, n.sim, seed = NULL, prior = NULL) {
    
    ## for rdirichlet
    require(MCMCpack)
    
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
    rn <- as.data.frame(rdirichlet(nsim, alpha = alpha))
    colnames(rn) <- survey$party  
    
    rn
    
}