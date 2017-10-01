#' Get probabilities to enter the parliament.
#'
#' @inheritParams get_seats
#' @param dirichlet.draws Matrix or data frame containing draws from
#' the posterior (see \code{\link{draw_from_posterior}}).
#' @return Vector of (named) entry probabilities.
#' @seealso \code{\link{draw_from_posterior}}
#' @keywords internal
#' @export
get_entryprobability <- function(dirichlet.draws, hurdle = 0.05) {

  colSums(dirichlet.draws >= hurdle) / nrow(dirichlet.draws)

}
