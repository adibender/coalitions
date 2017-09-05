#' Calculate seat distribution from draws from posterior
#'
#' @param dirichlet.draws Matrix containing random draws from posterior.
#' @param survey The actual survey results on which \code{dirichlet.draws}
#' were based on.
#' @param distrib.fun Function to calculate seat distribution. Defaults to
#' \code{\link{sls}} (Sainte-Lague/Scheppers).
#' @param samplesize Number of individuals participating in the \code{survey}.
#' @param hurdle The percentage which has to be reached to enter the
#' parliament.
#' @param others A character indicating the
#' @param ... Further arguments passed to \code{distrib.fun}.
#' @import checkmate dplyr
#' @return A data frame containing seat distributions for each simulation in
#' \code{dirichlet.draws}
#' @keywords seat distribution
#' @seealso \code{\link{draw_from_posterior}}, \code{\link{sls}},
#' \code{\link{dHondt}}
#' @export
get_seats <- function(
  dirichlet.draws,
  survey,
  distrib.fun = sls,
  samplesize  = NULL,
  hurdle      = 0.05,
  others      = "sonstige",
  ... ) {

  assert_data_frame(dirichlet.draws, any.missing=FALSE, min.rows=1, min.cols=2)
  assert_number(hurdle, lower=0, upper=1)
  assert_string(others, min.chars=1)

  if( is.null(samplesize) ) samplesize <- sum(survey$votes)

  pnames <- colnames(dirichlet.draws)

  dirichlet.draws %>%
    mutate(sim = row_number()) %>%
    gather(party, percent, one_of(pnames)) %>%
    arrange(sim) %>%
    mutate(votes = percent * samplesize) %>%
    filter(party != others & percent >= hurdle) %>%
    group_by(sim) %>%
    mutate(seats = distrib.fun(votes, party, ...)) %>%
    ungroup() %>%
    select(sim, party, seats)

}