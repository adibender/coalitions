#' Calculate seat distribution from draws from posterior
#' 
#' @param dirichlet.draws Matrix containing random draws from posterior.
#' @param survey The actual survey results on which \code{dirichlet.draws}
#' were based on.
#' @param distrib.fun Function to calculate seat distribution. Defaults to 
#' \code{\link{sls}} (Sainte-Lague/Scheppers). 
#' @param samplesize Number of individuals participating in the \code{survey}.
#' @param ... Further arguments passed to \code{distrib.fun}.
#' @return \code{list} containing seat distributions for each simulation in 
#' \code{dirichlet.draws}
#' @export
#' @keywords seat distribution
#' @seealso \code{\link{draw_from_posterior}}, \code{\link{sls}}, 
#' \code{\link{dHondt}}
get_seat_distribution <- function(dirichlet.draws, survey, distrib.fun = sls, 
  samplesize = NULL, ...) {
  
  if( is.null(samplesize) ) samplesize <- sum(survey$votes)
    sim.surveys <- lapply(seq_len(nrow(dirichlet.draws)), function(z) {
      
      survey$votes.in.perc <- as.numeric(dirichlet.draws[z, ])
      survey$votes <- survey$votes.in.perc * samplesize
      survey
      
    })
  
    ## calculate seat distribution for each simulation
  sim.results <- lapply(sim.surveys, distrib.fun, ...)
  
    ## return results
  sim.results
  
}


#' @rdname get_seat_distribution
#' @param mc.cores Number of cores to be used in parallel. 
#' See \code{\link[parallel]{mclapply}}.
get_seats <- function(
  dirichlet.draws, 
  survey, 
  mc.cores    = 1,
  distrib.fun = sls,
  samplesize  = NULL, ...) {
  
  if( is.null(samplesize) ) samplesize <- sum(survey$VOTES)
  
    sim.surveys <- mclapply(seq_len(nrow(dirichlet.draws)), function(z) {
      survey$PERCENT <- as.numeric(dirichlet.draws[z, ])
      survey$VOTES <- survey$PERCENT * samplesize
      survey
      
    }, mc.cores=mc.cores)
  
  ## calculate seat distribution for each simulation
  sim.results <- mclapply(sim.surveys, distrib.fun, ..., mc.cores=mc.cores)
  
  ## return results
  sim.results
  
}