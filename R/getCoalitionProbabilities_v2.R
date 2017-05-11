
#' @rdname get_coalition_probability
get_probability <- function(
  seat.tab, 
  coalition, 
  superior = NULL,
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
    COALITION   = sapply(coalitions          , paste0, collapse = "-"),
    SUPERIOR    = sapply(superior.coalitions , paste0, collapse = "-"),
    PROBABILITY = coal.probs)

}
