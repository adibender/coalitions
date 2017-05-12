#' Seat Distribution by Sainte-Lague/Scheppers
#' 
#' Calculates number of seats for the respective parties that have received more 
#' than 5\% of votes (according to the method of Sainte-Lague/Schepers,
#' see http://www.wahlrecht.de/verfahren/rangmasszahlen.html).
#' 
#' @inheritParams redistribute
#' @param survey Results of a survey as data.frame containing party names and votes.
#' @param seats Number of seats in parliament. Defaults to 598 (seats in german 
#' parliament).
#' @param epsilon The percentages of votes in survey must add up to 1, 
#' this allows for some numerical imprecission. Defaults to 10e-6.
#' @return A \code{data.frame} containing parties above the hurdle and the respective 
#' seats/percentages after redistribution via Sainte-Lague/Scheppers.
#' @import dplyr
#' @importFrom reshape2 melt
#' @export
#' @seealso \code{\link{dHondt}}
sls <- function(
  survey, 
  seats   = 598,
  hurdle  = 0.05,
  epsilon = 10e-6,
  others  = "sonstige") {

    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "others"
  survey <- redistribute(survey, hurdle = hurdle, others=others, epsilon=epsilon)

  div_vec <- seq(0.5, seats + 0.5, by = 1)
  divisor.mat <- sum(survey$votes)/vapply(survey$votes, "/", numeric(599), div_vec)
  colnames(divisor.mat) <- survey$party

  m.mat <- melt(divisor.mat, id.vars = "party", as.is=TRUE, value.name="seats")
  m.mat <- m.mat[rank(m.mat$seats, ties.method = "random") <= seats, ]
  rle.seats <- rle(m.mat$Var2)
  seat.mat <- bind_cols(rle.seats[2:1])
  colnames(seat.mat) <- c("party","seats")

  if( nrow(seat.mat) != nrow(survey) ) 
    stop ("Wrong number of parties after seat distribution")
  if( sum(seat.mat$seats) != seats ) 
    stop(paste("Number of seats distributed not equal to", seats))

  seat.mat

}

#' @inherit sls
sls2 <- function(
  votes,
  parties, 
  seats   = 598) {

  # div_vec <- seq(0.5, seats + 0.5, by = 1)
  # div_vec <- 0.5:(seats+0.5)
  # n1 <- seats+1
  divisor.mat <- sum(votes)/t(div_mat[seq_along(votes), ]*votes)
  colnames(divisor.mat) <- parties

  m.mat <- melt(divisor.mat, as.is=TRUE, value.name="seats")
  m.mat <- m.mat[rank(m.mat$seats, ties.method = "random") <= seats, ]
  rle.seats <- rle(m.mat$Var2)
  # seat.mat <- bind_cols(rle.seats[2:1])
  # colnames(seat.mat) <- c("party","seats")

  if( sum(rle.seats$length) != seats ) 
    stop(paste("Number of seats distributed not equal to", seats))

  rle.seats$length

}