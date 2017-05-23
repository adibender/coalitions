#' Calculate seat distribution from draws from posterior
#' 
#' @param dirichlet.draws Matrix containing random draws from posterior.
#' @param survey The actual survey results on which \code{dirichlet.draws}
#' were based on.
#' @param distrib.fun Function to calculate seat distribution. Defaults to 
#' \code{\link{sls}} (Sainte-Lague/Scheppers). 
#' @param samplesize Number of individuals participating in the \code{survey}.
#' @param ... Further arguments passed to \code{distrib.fun}.
#' @param mc.cores Number of cores to be used in parallel.
#' See \code{\link[parallel]{mclapply}}.
#' @inheritParams purrr:::map_dfr
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom parallel mclapply
#' @return \code{list} containing seat distributions for each simulation in 
#' \code{dirichlet.draws}
#' @keywords seat distribution
#' @seealso \code{\link{draw_from_posterior}}, \code{\link{sls}}, 
#' \code{\link{dHondt}}
#' @export 
get_seats <- function(
  dirichlet.draws, 
  survey, 
  mc.cores    = 1,
  distrib.fun = sls,
  samplesize  = NULL, 
  .id = "sim", ...) {
  
  if( is.null(samplesize) ) samplesize <- sum(survey$votes)

  percent_mat <- as.matrix(dirichlet.draws)
  votes_mat <- percent_mat * samplesize
  sim.surveys <- mclapply(seq_len(nrow(percent_mat)), 
    function(z) {
      survey$percent <- percent_mat[z, ]
      survey$votes   <- votes_mat[z, ]
      survey 
    }, mc.cores=mc.cores)
  
  ## calculate seat distribution for each simulation
 map_dfr(sim.surveys, distrib.fun, .id=.id)
  
}


#' @inherit get_seats
get_seats2 <- function(
  dirichlet.draws, 
  survey, 
  distrib.fun = sls2,
  samplesize  = NULL,
  .id         = "sim",
  hurdle      = 0.05,
  others      = "sonstige",
  ... ) {

    if( is.null(samplesize) ) samplesize <- sum(survey$votes)

    dirichlet.draws %>% 
      mutate(sim = row_number()) %>% 
      gather(party, percent, cdu:sonstige) %>% 
      arrange(sim) %>% 
      mutate(votes = percent * samplesize) %>% 
      filter(party != others & percent >= hurdle) %>% 
      group_by(sim) %>% 
      mutate(seats = distrib.fun(votes, party)) %>% 
      ungroup() %>% 
      select(sim, party, seats)

}