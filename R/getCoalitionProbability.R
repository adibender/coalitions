#' Calculates coalition probability for one coalition given seat distribution

#' @param seat.tab \code{data.frame} containing seat.distributions from simulations
#' @param coalition the coalition of interest
#' @param superior Superior coalition to \code{coalition}. If not \code{NA}
#' majority for coalition is considered only if there is no majority for 
#' \code{superior} coalition.
#' @param majority Number of seats required for majority in parliament. 
#' Defaults to 300 (required number of seats in german parliament).

#' @return Probability for majority of \code{coalition}.

#' @seealso \code{\link{}}

#' @export

get_coalition_probability <- function(seat.tab, coalition, superior = NULL, 
    majority = 300) {
    
    ind.coalition <- sapply(seat.tab, function(z) {
                sum(z$seats[z$party %in% coalition]) >= majority
            })
    
    if( !any(is.null(superior)) ) {
        
        ind.sup.list <- lapply(superior, function(superior.coalition) {
                    sapply(seat.tab, function(z) {
                                sum(z$seats[z$party %in% superior.coalition]) >= 
                                        majority
                            })
                })
    }
    else{
        ind.sup.list <- list(rep(FALSE, length(ind.coalition)))
    }
    
    ind.sup <- Reduce("|", ind.sup.list)
    
    mean(ind.coalition & !(ind.sup))
    
}


get_coalition_probabilities <- function(seat.tabs, coalitions, 
    superior.coalitions, majority = 300) {
    
    coal.probs <- sapply(seq_along(coalitions), function(z) {
                get_coalition_probability(seat.tabs, coalitions[[z]], 
                        superior.coalitions[[z]], majority = majority)
            })  
    names(coal.probs) <- sapply(coalitions, paste0, collapse = "-")
    
    coal.probs
    
}


get_lentvote_probabilities <- function(dirichlet.draws, survey,  
        coalitions, majority = 300, max.percent.lent = 10, 
        distrib.fun = sls,
        superior.coalitions = NULL) {
    
    if( is.null(superior.coalitions)  ) 
        superior.coalitions <- as.list(rep(NA, length(coalitions)))
    
    
    survey.tab <- as_survey(survey$Anteil/100, unique(survey$Befragte), 
            survey$Partei)
    
    votes.lent <- 0:max.percent.lent / 100# vektor zu betrachtender anteile 
    # geliehener Stimmen
    
    ## berechne für jeden zu betrachtenden anteil geliehener stimmen die sitzeverteilung
    rn.lent.list <- lapply(votes.lent, lentVotesRn, rn.mat = dirichlet.draws)
    
    seat.lent.list <- lapply(rn.lent.list, getSeatDistribution, survey = survey.tab, 
            distrib.fun = distrib.fun)
    
    ## berechne koalitionswahrscheinlichkeiten etc.
    probs.list <- lapply(seat.lent.list, getCoalitionProbs, 
            coalitions = coalitions, 
            superior.coalitions = superior.coalitions, 
            majority = majority)
    names(probs.list) <- votes.lent
    probs.list <- lapply(probs.list, function(z) cbind.data.frame(Koalition = names(z), 
                        Anteil = z))
    
    m.cp <- melt(probs.list, id.vars = "Koalition")
    colnames(m.cp)[grep("L1", colnames(m.cp))] <- "lent"
    m.cp$Institut <- unique(survey$Institut)
    m.cp$Datum <- unique(survey$Datum)
    m.cp$Befragte <- unique(survey$Befragte)
    
    m.cp
    
}