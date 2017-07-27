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
dHondt <- function(survey, hurdle = 0.04, seats = 183, epsilon = 1e-6) {

    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "Sonstige"
    survey <- redistribute(survey, hurdle = hurdle)

    # check for data validity
    if( abs(sum(survey$votes.in.perc) - 1) > epsilon  )
        stop("Percentages of votes provided to dHondt() function don't add up to 1!")

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
