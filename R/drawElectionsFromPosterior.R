#' Draw random numbers from posterior distribution
#'
#' @param survey survey object as returned by \code{as_survey} or \code{getSurveys}
#' @param nsim number of simulations
#' @param seed sets seed
#' @param prior optional prior information. Defaults to 1/2 (Jeffrey's prior).
#' @importFrom gtools rdirichlet
#' 
#' @return \code{data.frame} containing random draws from dirichlet distribution
#' which can be interpreted as election results.
#' @export
#' @keywords draw, simulate
#' @seealso \code{\link{as_survey}}

draw_from_posterior <- function(survey, nsim, seed = NULL, prior = NULL) {

    ## calculate posteriori
    if( is.null(prior) ) {
        prior <- rep(0.5, nrow(survey))
    }

    else {
        if( length(prior) != nrow(survey) )
            stop("length of prior weights and number of observations differ")
    }

    alpha <- survey$votes + prior

    ## draw n.sim random dirichlet numbers/vectors with concentration weights alpha
    if( !is.null(seed) ) set.seed(seed)
    rn <- as.data.frame(rdirichlet(nsim, alpha = alpha))
    colnames(rn) <- survey$party

    rn

}
