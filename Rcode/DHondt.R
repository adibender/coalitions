dHondt <- function(survey, hurdle = 0.04, seats = 183, epsilon = 1e-6) {
    
    require(reshape2)
    
    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "Sonstige"
    survey <- get.props(survey, hurdle = hurdle)
    
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