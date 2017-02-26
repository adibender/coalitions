#' Calculates coalition probability for one coalition given seat distribution
#'
#' @param seat.tab \code{data.frame} containing seat.distributions from simulations
#' @param coalition the coalition of interest
#' @param superior Superior coalition to \code{coalition}. If not \code{NA}
#' majority for coalition is considered only if there is no majority for
#' \code{superior} coalition.
#' @param majority Number of seats required for majority in parliament.
#' Defaults to 300 (required number of seats in german parliament).
#' @importFrom reshape2 melt
#' @return Probability for majority of \code{coalition}.
#' @export
#' @seealso \code{\link{get_entryprobability}}

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

#' @rdname get_coalition_probability
get_probability <- function(
  seat.tab, 
  coalition, 
  superior = NULL,
  majority = 300) {

  ind.coalition <- sapply(seat.tab, function(z) {
    sum(z$SEATS[z$PARTY %in% coalition]) >= majority
  })

  if( !any(is.null(superior)) ) {

    ind.sup.list <- lapply(superior, function(superior.coalition) {
      sapply(seat.tab, function(z) {
        sum(z$SEATS[z$PARTY %in% superior.coalition]) >=
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


#' Calculate coalition probabilities
#' 
#' Given a list containing simulated seats obtained per party
#' 
#' @param seat.tabs A list of data frames containing simulated seat distributions. 
#' @param coalitions A list of coaltions for which coalition probabilities should 
#' be calculated. Each list entry must be a vector of party names. Those names 
#' need to correspond to the names in \code{seat.tabs}.
#' @param superior.coalitions A list similar to \code{coalitions} and of the 
#' same length. Coalition probabilities for coalitions in \code{coalitions} list 
#' will be calculated excluding times where respective entry in 
#' \code{superior.coalitions} is also possible.
#' @inheritParams get_coalition_probability
#' @return Probabilities for majority for specified coalitions.
#' @export
#' @seealso get_seat_distribution
get_coalition_probabilities <- function(
  seat.tabs, 
  coalitions,
  superior.coalitions, 
  majority = 300) {

  coal.probs <- sapply(seq_along(coalitions), function(z) {
    get_coalition_probability(seat.tabs, coalitions[[z]],
      superior.coalitions[[z]], majority = majority)
  })
  names(coal.probs) <- sapply(coalitions, paste0, collapse = "-")

  coal.probs

}

#' @rdname get_coalition_probabilities
get_probabilities <- function(
  seat.tabs, 
  coalitions,
  superior.coalitions, 
  majority = 300) {

  coal.probs <- sapply(seq_along(coalitions), function(z) {
    get_probability(seat.tabs, coalitions[[z]],
      superior.coalitions[[z]], majority = majority)
  })
  tibble(
    COALITION   = sapply(coalitions, paste0, collapse          = "-"),
    SUPERIOR    = sapply(superior.coalitions, paste0, collapse = "-"),
    PROBABILITY = coal.probs)

}



# #' @importFrom reshape2 melt
#
# get_lentvote_probabilities <- function(dirichlet.draws, survey,
#         coalitions, majority = 300, max.percent.lent = 10,
#         distrib.fun = sls,
#         superior.coalitions = NULL) {
#
#     if( is.null(superior.coalitions)  )
#         superior.coalitions <- as.list(rep(NA, length(coalitions)))
#
#
#     survey.tab <- as_survey(survey$Anteil/100, unique(survey$Befragte),
#             survey$Partei)
#
#     votes.lent <- 0:max.percent.lent / 100# vektor zu betrachtender anteile
#     # geliehener Stimmen
#
#     ## berechne für jeden zu betrachtenden anteil geliehener stimmen die sitzeverteilung
#     rn.lent.list <- lapply(votes.lent, lentVotesRn, rn.mat = dirichlet.draws)
#
#     seat.lent.list <- lapply(rn.lent.list, get_seat_distribution, survey = survey.tab,
#             distrib.fun = distrib.fun)
#
#     ## berechne koalitionswahrscheinlichkeiten etc.
#     probs.list <- lapply(seat.lent.list, get_coalition_probabilities,
#             coalitions = coalitions,
#             superior.coalitions = superior.coalitions,
#             majority = majority)
#     names(probs.list) <- votes.lent
#     probs.list <- lapply(probs.list, function(z) cbind.data.frame(Koalition = names(z),
#                         Anteil = z))
#
#     m.cp <- melt(probs.list, id.vars = "Koalition")
#     colnames(m.cp)[grep("L1", colnames(m.cp))] <- "lent"
#     m.cp$Institut <- unique(survey$Institut)
#     m.cp$Datum <- unique(survey$Datum)
#     m.cp$Befragte <- unique(survey$Befragte)
#
#     m.cp
#
# }
