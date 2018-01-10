#' Seat Distribution by Hare/Niemeyer
#'
#' Calculates number of seats for the respective parties that have received more
#' than \code{hurdle} percent of votes (according to the method of Hare/Niemeyer)
#'
#' @param votes Number of votes per party.
#' @param parties Names of parties (must be same length as votes).
#' @param n_seats Number of seats in parliament. Defaults to 183 (seats in
#' austrian parliament).
#' @seealso \code{\link{sls}}
#' @importFrom reshape2 melt
#' @return A \code{data.frame} containing parties above the hurdle and the respective
#' seats/percentages after redistribution via Hare/Niemeyer
#' @export
hare_niemeyer <- function(votes, parties, n_seats = 183) {

  quotas <- votes / sum(votes) * n_seats
  seats <- floor(quotas)
  rest <- quotas - base_seats
  seats_left <- n_seats - sum(seats)
  for (i in 1:seats_left) {
    m_i <- which.max(rest)
    if (length(m_i) > 1)
      m_i <- sample(m_i, 1)
    seats[m_i] <- seats[m_i] + 1
    rest[m_i] <- 0
  }
  seats
  
}
