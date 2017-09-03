#' Seat Distribution by Sainte-Lague/Scheppers
#'
#' Calculates number of seats for the respective parties that have received more
#' than 5\% of votes (according to the method of Sainte-Lague/Schepers,
#' see http://www.wahlrecht.de/verfahren/rangmasszahlen.html).
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

  ## attention: .div_mat is an internal object. see data_raw/internals.R
  divisor.mat <- sum(votes)/t(.div_mat[seq_along(votes), ]*votes)
  colnames(divisor.mat) <- parties

  m.mat <- melt(divisor.mat, as.is=TRUE, value.name="seats")
  m.mat <- m.mat[rank(m.mat$seats, ties.method = "random") <= n_seats, ]
  rle.seats <- rle(m.mat$Var2)
  # seat.mat <- bind_cols(rle.seats[2:1])
  # colnames(seat.mat) <- c("party","seats")

  if( sum(rle.seats$length) != n_seats )
    stop(paste("Number of seats distributed not equal to", seats))

  rle.seats$length

}


# sls <- function(
#   survey,
#   seats   = 598,
#   hurdle  = 0.05,
#   epsilon = 10e-6,
#   others  = "sonstige") {

#     #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "others"
#   survey <- redistribute(survey, hurdle = hurdle, others=others, epsilon=epsilon)

#   div_vec <- seq(0.5, seats + 0.5, by = 1)
#   divisor.mat <- sum(survey$votes)/vapply(survey$votes, "/", numeric(599), div_vec)
#   colnames(divisor.mat) <- survey$party

#   m.mat <- melt(divisor.mat, id.vars = "party", as.is=TRUE, value.name="seats")
#   m.mat <- m.mat[rank(m.mat$seats, ties.method = "random") <= seats, ]
#   rle.seats <- rle(m.mat$Var2)
#   seat.mat <- bind_cols(rle.seats[2:1])
#   colnames(seat.mat) <- c("party","seats")

#   if( nrow(seat.mat) != nrow(survey) )
#     stop ("Wrong number of parties after seat distribution")
#   if( sum(seat.mat$seats) != seats )
#     stop(paste("Number of seats distributed not equal to", seats))

#   seat.mat

# }