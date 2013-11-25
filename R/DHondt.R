#' Seat Distribution by D'Hondt
#' calculates number of seats for the respective parties that have received more 
#' than 5\% of votes (according to the method of D'Hondt)

#' @param survey results of a survey as data.frame containing party names and votes
#' @param seats number of seats in parliament. Defaults to 183 (seats in austrian 
#' parliament)
#' @param hurdle the percentage of votes that must be reached to get seats in 
#' parliament. Defaults to 0.04 (hurdle for austrian parliament)
#' @param epsilon the percentages of votes in survey must add up to 1, 
#' this allows for some numerical imprecission. Defaults to 10e-6.

#' @return data.frame containing parties above the hurdle and the respective 
#' seats/percentages after redistribution via Sainte-Lague/Scheppers

#' @seealso \code{\link{sls}}

#' @export


dHondt <- function(survey, hurdle = 0.04, seats = 183, epsilon = 1e-6) {

    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "Sonstige"
    survey <- redistribute(survey, hurdle = hurdle)
    
    # check for data validity
    if( abs(sum(survey$votes.in.perc) - 1) > epsilon  ) 
        stop("wrong percentages provided in dHondt() function")
    
    divisor.mat <- sum(survey$votes)/sapply(survey$votes, "/",  seq(1, 183, by = 1))
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