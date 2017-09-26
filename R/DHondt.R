#' Seat Distribution by D'Hondt
#'
#' Calculates number of seats for the respective parties that have received more
#' than \code{hurdle} percent of votes (according to the method of D'Hondt)
#'
#' @param survey Results of a survey as data.frame containing party names and votes.
#' @param seats Number of seats in parliament. Defaults to 183 (seats in austrian
#' parliament).
#' @param hurdle The percentage of votes that must be reached to get seats in
#' parliament. Defaults to 0.04 (hurdle for austrian parliament).
#' @param epsilon The percentages of votes in survey must add up to 1,
#' this allows for some numerical imprecission. Defaults to 10e-6.
#' @seealso \code{\link{sls}}
#' @importFrom reshape2 melt
#' @export
#' @return A \code{data.frame} containing parties above the hurdle and the respective
#' seats/percentages after redistribution via D'Hondt
dHondt <- function(votes, parties, n_seats = 183) {

  divisor.mat           <- sum(votes)/sapply(votes, "/", seq(1, n_seats, 1))
  colnames(divisor.mat) <- parties
  m.mat                 <- melt(divisor.mat, id.vars = "party")
  m.mat                 <- m.mat[rank(m.mat$value, ties.method = "random") <= n_seats, ]
  rle.seats             <- rle(as.character(m.mat$Var2))

  if( sum(rle.seats$length) != n_seats )
    stop(paste("Number of seats distributed not equal to", n_seats))

  rle.seats$length

}
