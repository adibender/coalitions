\name{draw_from_posterior}
\alias{draw_from_posterior}
\title{Draw random numbers from posterior distribution}
\usage{
  draw_from_posterior(survey, nsim, seed = NULL,
    prior = NULL)
}
\arguments{
  \item{survey}{survey object as returned by
  \code{as_survey} or \code{getSurveys}}

  \item{nsim}{number of simulations}

  \item{seed}{sets seed}

  \item{prior}{optional prior information. Defaults to 1/2
  (Jeffrey's prior).}
}
\value{
  \code{data.frame} containing random draws from dirichlet
  distribution which can be interpreted as election
  results.
}
\description{
  Draw random numbers from posterior distribution
}
\seealso{
  \code{\link{as_survey}}
}
\keyword{draw,}
\keyword{simulate}

