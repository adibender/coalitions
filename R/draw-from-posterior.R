#' Draw random numbers from posterior distribution
#'
#' @param survey survey object as returned by \code{as_survey} or \code{getSurveys}
#' @param nsim number of simulations
#' @param seed sets seed
#' @param prior optional prior information. Defaults to 1/2 (Jeffrey's prior).
#' @importFrom gtools rdirichlet
#' @importFrom dplyr tbl_df
#' @importFrom lubridate now
#' @return \code{data.frame} containing random draws from dirichlet distribution
#' which can be interpreted as election results.
#' @keywords draw, simulate
#' @seealso \code{\link{as_survey}}
#' @export
draw_from_posterior <- function(
  survey, 
  nsim  = 1e4,
  seed  = as.numeric(now()),
  prior = NULL) {

  ## set seed if provided
  if(!is.null(seed)) set.seed(seed)
    ## calculate posteriori
  if(is.null(prior)) {
    prior <- rep(0.5, nrow(survey))
  } else {
    if(length(prior) != nrow(survey))
      stop("length of prior weights and number of observations differ")
  }
    
  ## draw n.sim random dirichlet numbers/vectors with concentration weights alpha
  draws <- rdirichlet(nsim, alpha = survey$votes + prior)
  colnames(draws) <- survey$party

  return(tbl_df(draws))

}