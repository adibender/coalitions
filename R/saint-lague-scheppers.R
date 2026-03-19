#' Seat Distribution by Sainte-Lague/Schepers
#'
#' Calculates number of seats for the respective parties that have received more
#' than 5\% of votes (according to the method of Sainte-Lague/Schepers,
#' see https://www.wahlrecht.de/verfahren/rangmasszahlen.html).
#'
#' @param votes A numeric vector giving the redistributes votes
#' @param parties A character vector indicating the names of parties with
#' respective \code{votes}.
#' @param n_seats The total number of seats that can be assigned to the different
#' parties.
#' @return A named integer vector of seat counts, one entry per party,
#' in the same order as \code{parties}. The vector has a logical attribute
#' \code{ties}: \code{TRUE} if two or more parties had equal claim to the last
#' seat (i.e. the result is not uniquely determined and was resolved randomly),
#' \code{FALSE} otherwise. When \code{ties = TRUE}, re-running with a different
#' random seed may produce a different but equally valid seat distribution.
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @seealso \code{\link{dHondt}}
#' @examples
#' library(coalitions)
#' library(dplyr)
#' # get the latest survey for a sample of German federal election polls
#' surveys <- get_latest(surveys_sample) %>% ungroup() %>% slice(1) %>% tidyr::unnest("survey")
#' # calculate the seat distribution based on Sainte-Lague/Schepers for a parliament with 300 seats
#' sls(surveys$votes, surveys$party, n_seats = 300)
#' @export
sls <- function(
  votes,
  parties,
  n_seats   = 598L) {

  # attention: .div_mat is an internal object. see data_raw/internals.R
  divisor.mat <- sum(votes) / t(.div_mat[seq_along(votes), ] * votes)
  colnames(divisor.mat) <- parties
  m.mat <- tidyr::pivot_longer(as.data.frame(divisor.mat), cols = everything(),
    names_to = "name", values_to = "seats")

  sorted_vals <- sort(m.mat$seats, decreasing = TRUE)
  has_ties <- length(sorted_vals) > n_seats &&
    sorted_vals[n_seats] == sorted_vals[n_seats + 1L]

  m.mat     <- m.mat[rank(m.mat$seats, ties.method = "random") <= n_seats, ]
  m.mat     <- m.mat[order(m.mat$name), ]
  rle.seats <- rle(m.mat$name)

  if (sum(rle.seats$length) != n_seats)
    stop(paste("Number of seats distributed not equal to", n_seats))

  result <- rle.seats$length
  attr(result, "ties") <- has_ties
  result

}
