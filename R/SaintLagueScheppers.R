#' Seat Distribution by Sainte-Lague/Scheppers
#' 
#' Calculates number of seats for the respective parties that have received more 
#' than 5\% of votes (according to the method of Sainte-Lague/Schepers,
#' see http://www.wahlrecht.de/verfahren/rangmasszahlen.html).

#' @param survey Results of a survey as data.frame containing party names and votes.
#' @param seats Number of seats in parliament. Defaults to 598 (seats in german 
#' parliament).
#' @param hurdle The percentage of votes that must be reached to get seats in 
#'  parliament. Defaults to 0.05 (hurdle for german parliament).
#' @param epsilon The percentages of votes in survey must add up to 1, 
#' this allows for some numerical imprecission. Defaults to 10e-6.

#' @return A \code{data.frame} containing parties above the hurdle and the respective 
#' seats/percentages after redistribution via Sainte-Lague/Scheppers.

#' @seealso \code{\link{dHondt}}

#' @export

sls <- function(survey, seats = 598, hurdle = 0.05, epsilon = 10e-6) {
    
    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "others"
    survey <- redistribute(survey, hurdle = hurdle)
    
    # check for data validity
    if( abs(sum(survey$votes.in.perc) - 1) > epsilon  ) 
        stop("wrong percentages provided in sls() function")
    
    divisor.mat <- sum(survey$votes)/sapply(survey$votes, "/",  
            seq(0.5, 598.5, by = 1))
    colnames(divisor.mat) <- survey$party
    
    m.mat <- melt(divisor.mat, id.vars = "party")
    m.mat <- m.mat[rank(m.mat$value, ties.method = "random") <= seats, ]
    rle.seats <- rle(as.character(m.mat$Var2))
    seat.mat <- cbind.data.frame(party = rle.seats$values, seats = rle.seats$lengths)
    
    if( nrow(seat.mat) != nrow(survey) ) 
        stop ("Wrong number of parties after seat distribution")
    if( sum(seat.mat$seats) != seats ) 
        stop(paste("Number of seats distributed not equal to", seats))
    
    survey <- merge(survey, seat.mat, by = "party")
    
    survey
    
}