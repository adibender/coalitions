#' Calculate seat distribution from draws from posterior
#'
#' @param dirichlet.draws Matrix containing random draws from posterior.
#' @param survey The actual survey results on which \code{dirichlet.draws}
#' were based on.
#' @param distrib.fun Function to calculate seat distribution. Defaults to
#' \code{\link{sls}} (Sainte-Lague/Schepers).
#' @param samplesize Number of individuals participating in the \code{survey}.
#' @param hurdle The percentage threshold which has to be reached by a party
#' to enter the parliament.
#' @param others A string indicating the name under which parties not listed
#' explicitly are subsumed.
#' @param ... Further arguments passed to \code{distrib.fun}.
#' @import checkmate dplyr
#' @return A data frame containing seat distributions for each simulation in
#' \code{dirichlet.draws}
#' @keywords seat distribution
#' @seealso \code{\link{draw_from_posterior}}, \code{\link{sls}},
#' \code{\link{dHondt}}
#' @examples 
#' library(coalitions)
#' library(dplyr) 
#' # scrape the newest survey from the Emnid polling agency
#' surveys <- get_surveys() %>% filter(pollster == "emnid") %>% tidyr::unnest() %>% slice(1)
#' # simulate 100 seat distributions
#' surveys <- surveys %>% mutate(draws = purrr::map(survey, draw_from_posterior, nsim = 100),
#'                               seats = purrr::map2(draws, survey, get_seats))
#' surveys$seats
#' @export
get_seats <- function(
  dirichlet.draws,
  survey,
  distrib.fun = sls,
  samplesize  = NULL,
  hurdle      = 0.05,
  others      = "others",
  ... ) {

  assert_data_frame(dirichlet.draws, any.missing = FALSE, min.rows = 1,
    min.cols = 2)
  assert_number(hurdle, lower = 0, upper = 1)
  assert_string(others, min.chars = 1)

  if (is.null(samplesize)) samplesize <- sum(survey$votes)

  pnames <- colnames(dirichlet.draws)

  dirichlet.draws %>%
    mutate(sim = row_number()) %>%
    gather("party", "percent", -.data$sim) %>%
    arrange(.data$sim) %>%
    mutate(votes = .data$percent * samplesize) %>%
    filter(.data$party != others & .data$percent >= hurdle) %>%
    group_by(.data$sim) %>%
    mutate(seats = distrib.fun(.data$votes, .data$party, ...)) %>%
    ungroup() %>%
    select(one_of(c("sim", "party", "seats")))

}

#' Calculate percentage of votes/seats after excluding parties with
#' \code{votes < hurdle}
#'
#' @inheritParams get_seats
#' @param epsilon Percentages should add up to 1. If they do not, within accuracy
#' of \code{epsilon}, an error is thrown.
#' @seealso \code{\link{get_seats}}, \code{\link{sls}}
#' @examples
#' library(coalitions)
#' library(dplyr) 
#' # scrape the newest survey from the Emnid polling agency
#' surveys <- get_surveys() %>% filter(pollster == "emnid") %>% tidyr::unnest() %>% slice(1)
#' # redistribute the shares of 'others' parties and parties with a share of under 5\%
#' surveys <- surveys %>% mutate(survey_redist = purrr::map(survey, redistribute))
#' surveys$survey # results before redistribution
#' surveys$survey_redist # results after redistribution
#' @export
redistribute <- function(
  survey,
  hurdle  = 0.05,
  others  = "others",
  epsilon = 10e-6) {

  # must be & (we include parties with percent > hurdle and name != others)
  survey <- survey[survey$percent >= hurdle & survey$party != others, ]

  survey$percent <- survey$votes / sum(survey$votes)

  # check for data validity
  if (abs(sum(survey$percent) - 1) > epsilon)
    stop("wrong percentages calculated in 'redistribute()' function")

  survey

}
