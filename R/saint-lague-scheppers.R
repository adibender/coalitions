#' Seat Distribution by Sainte-Lague/Schepers
#'
#' Calculates number of seats for the respective parties that have received more
#' than 5\% of votes (according to the method of Sainte-Lague/Schepers,
#' see https://www.wahlrecht.de/verfahren/rangmasszahlen.html).
#'
#' @inheritParams redistribute
#' @param votes A numeric vector giving the redistributes votes
#' @param parties A character vector indicating the names of parties with
#' respective \code{votes}.
#' @param n_seats The total number of seats that can be assigned to the different
#' parties.
#' @return A numeric vector giving the number of seats each party obtained.
#' @import dplyr
#' @importFrom reshape2 melt
#' @seealso \code{\link{dHondt}}
#' @export
sls <- function(
  votes,
  parties,
  n_seats   = 598L) {

  # attention: .div_mat is an internal object. see data_raw/internals.R
  divisor.mat <- sum(votes) / t(.div_mat[seq_along(votes), ] * votes)
  colnames(divisor.mat) <- parties

  m.mat     <- melt(divisor.mat, as.is = TRUE, value.name = "seats")
  m.mat     <- m.mat[rank(m.mat$seats, ties.method = "random") <= n_seats, ]
  rle.seats <- rle(m.mat$Var2)

  if (sum(rle.seats$length) != n_seats)
    stop(paste("Number of seats distributed not equal to", n_seats))

  rle.seats$length

}
