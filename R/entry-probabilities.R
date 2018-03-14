#' Get probabilities to enter the parliament.
#'
#' @inheritParams get_seats
#' @param dirichlet.draws Matrix or data frame containing draws from
#' the posterior (see \code{\link{draw_from_posterior}}).
#' @return Vector of (named) entry probabilities.
#' @seealso \code{\link{draw_from_posterior}}
#' @keywords internal
#' library(coalitions)
#' library(dplyr) 
#' # scrape the newest survey from the Emnid polling agency
#' surveys <- get_surveys() %>% filter(pollster == "emnid") %>% tidyr::unnest() %>% slice(1)
#' # use 100 simulations for a fast runtime
#' surveys <- surveys %>% mutate(draws = purrr::map(survey, draw_from_posterior, nsim = 100),
#'                               entryProbs = purrr::map(draws, get_entryprobability))
#' surveys$entryProbs
#' @export
get_entryprobability <- function(dirichlet.draws, hurdle = 0.05) {

  colSums(dirichlet.draws >= hurdle) / nrow(dirichlet.draws)

}
