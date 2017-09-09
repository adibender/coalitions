#' Draw random numbers from posterior distribution
#'
#' @param survey survey object as returned by \code{as_survey} or \code{getSurveys}
#' @param nsim number of simulations
#' @param seed sets seed
#' @param prior optional prior information. Defaults to 1/2 (Jeffrey's prior).
#' @param correction A positiv number. If not \code{NULL}, each sample from the
#' dirichlet distribution will be additionally "corrected" by a random number
#' from U(-1*correction, 1*correction). This can be used to introduce extra
#' variation which might be usefull due to rounding errors from reported survey
#' results (or add an aditional source of variation in general).
#'
#' @importFrom gtools rdirichlet
#' @importFrom dplyr tbl_df
#' @importFrom lubridate now
#' @importFrom stats runif
#' @return \code{data.frame} containing random draws from dirichlet distribution
#' which can be interpreted as election results.
#' @keywords draw, simulate
#' @seealso \code{\link{as_survey}}
#' @export
draw_from_posterior <- function(
  survey,
  nsim       = 1e4,
  seed       = as.numeric(now()),
  prior      = NULL,
  correction = NULL) {

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

  if (!is.null(correction)) {
    draws_correction <- matrix(
      runif(prod(dim(draws)), -1*correction, 1*correction),
      nrow = nrow(draws),
      ncol = ncol(draws))
    draws_correction <- draws_correction - rowMeans(draws_correction)
    draws <- draws + draws_correction

    if (any(ind.mat <- draws < 0)) {
      draws <- draws[!(rowSums(ind.mat) > 0), , drop=FALSE]
      warning(paste0(
        "Some drawn percentages were smaller than 0.\n ",
        "The value of the correction may be to large.\n ",
        "Draws with negative percentages will be excluded, which
        may leed to bias."))
    }
  }


  return(tbl_df(draws))

}