#' Seat Distribution by D'Hondt
#'
#' Calculates number of seats for the respective parties according to the
#' method of d'Hondt.
#'
#' @param votes Number of votes per party.
#' @param parties Names of parties (must be same length as votes).
#' @param n_seats Number of seats in parliament. Defaults to 183 (seats in
#' Austrian parliament).
#' @seealso \code{\link{sls}}
#' @importFrom tidyr gather
#' @return A numeric vector containing the seats of all parties after
#' redistribution via D'Hondt
#' @examples
#' library(coalitions)
#' library(dplyr)
#' # get the latest survey for a sample of German federal election polls
#' surveys <- get_latest(surveys_sample) %>% tidyr::unnest("survey")
#' # calculate the seat distribution based on D'Hondt for a parliament with 300 seats
#' dHondt(surveys$votes, surveys$party, n_seats = 300)
#' @export
dHondt <- function(votes, parties, n_seats = 183) {

  divisor.mat           <- sum(votes) / sapply(votes, "/", seq(1, n_seats, 1))
  colnames(divisor.mat) <- parties

  m.mat     <- tidyr::gather(as.data.frame(divisor.mat), key="name", value="value",
    everything())
  m.mat     <- m.mat[rank(m.mat$value, ties.method = "random") <= n_seats, ]
  rle.seats <- rle(as.character(m.mat$name))

  if (sum(rle.seats$length) != n_seats)
    stop(paste("Number of seats distributed not equal to", n_seats))

  # fill up the vector with parties that got no seats
  if (any(!(parties %in% rle.seats$values))) {
    # add parties
    missing_parties <- parties[!(parties %in% rle.seats$values)]
    for (party in missing_parties) {
      rle.seats$lengths <- c(rle.seats$lengths, 0)
      rle.seats$values  <- c(rle.seats$values, party)
    }
    # sort results
    rle.seats$lengths <- rle.seats$lengths[match(parties, rle.seats$values)]
    rle.seats$values  <- rle.seats$values[match(parties, rle.seats$values)]
  }
  
  rle.seats$length

}
