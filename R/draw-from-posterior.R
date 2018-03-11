#' Draw random numbers from posterior distribution
#'
#' @param survey survey object as returned by \code{as_survey} or \code{getSurveys}
#' @param nsim number of simulations
#' @param seed sets seed
#' @param prior optional prior information. Defaults to 1/2 (Jeffrey's prior).
#' @param correction A positive number. If not \code{NULL}, each sample from the
#' Dirichlet distribution will be additionally "corrected" by a random number
#' from U(-1*correction, 1*correction). This can be used to introduce extra
#' variation which might be useful due to rounding errors from reported survey
#' results (or add an additional source of variation in general).
#'
#' @importFrom gtools rdirichlet
#' @importFrom dplyr tbl_df
#' @importFrom lubridate now
#' @importFrom stats runif
#' @return \code{data.frame} containing random draws from Dirichlet distribution
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
  if (!is.null(seed)) set.seed(seed)
    ## calculate posteriori
  if (is.null(prior)) {
    prior <- rep(0.5, nrow(survey))
  } else {
    if (length(prior) != nrow(survey))
      stop("length of prior weights and number of observations differ")
  }
  n_votes <- sum(survey$votes)
  percent <- survey$percent/100
  # draw n.sim random Dirichlet numbers/vectors with concentration weights alpha
  # draws <- rdirichlet(nsim, alpha = survey$votes + prior)
  # colnames(draws) <- survey$party

  if (!is.null(correction)) {
    corrections <- seq_len(nrow(survey)) %>%
      map(~runif(n=nsim, min=-1*correction, max=1*correction)) %>%
      do.call(what=cbind)
    corrections <- corrections - rowMeans(corrections)
    corrections <- t(t(corrections) + percent)
    # draws_correction <- matrix(
    #   runif(nsim * nrow(survey), -1 * correction, 1 * correction),
    #   nrow = nsim,
    #   ncol = nrow(survey))
    # draws_correction <- draws_correction - rowMeans(draws_correction)
    # alpha <- t(apply(draws_correction, 1, function(x) {x + percent}))
    if (any(ind.mat <- corrections < 0)) {
      corrections <- corrections[!(rowSums(ind.mat) > 0), , drop = FALSE]
      warning(paste0(
        "Some drawn percentages were smaller than 0.\n ",
        "The value of the correction may be to large.\n ",
        "Draws with negative percentages will be excluded, which may lead to bias."))
    }

    alpha <- corrections * n_votes + prior
    draws <- t(apply(alpha, 1, rdirichlet, n=1))

  } else {
    alpha <- survey$votes + prior
    draws <- rdirichlet(nsim, alpha)
  }

  colnames(draws) <- survey$party
  tbl_df(draws)

}
