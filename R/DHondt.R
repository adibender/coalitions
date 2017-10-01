#' Seat Distribution by D'Hondt
#'
#' Calculates number of seats for the respective parties that have received more
#' than \code{hurdle} percent of votes (according to the method of D'Hondt)
#'
#' @param votes Number of votes per party.
#' @param parties Names of parties (must be same length as votes).
#' @param n_seats Number of seats in parliament. Defaults to 183 (seats in
#' austrian parliament).
#' @seealso \code{\link{sls}}
#' @importFrom reshape2 melt
#' @return A \code{data.frame} containing parties above the hurdle and the respective
#' seats/percentages after redistribution via D'Hondt
#' @export
dHondt <- function(votes, parties, n_seats = 183) {

  divisor.mat           <- sum(votes) / sapply(votes, "/", seq(1, n_seats, 1))
  colnames(divisor.mat) <- parties
  m.mat     <- melt(divisor.mat, id.vars = "party")
  m.mat     <- m.mat[rank(m.mat$value, ties.method = "random") <= n_seats, ]
  rle.seats <- rle(as.character(m.mat$Var2))

  if (sum(rle.seats$length) != n_seats)
    stop(paste("Number of seats distributed not equal to", n_seats))

  rle.seats$length

}
