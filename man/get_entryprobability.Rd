% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entry-probabilities.R
\name{get_entryprobability}
\alias{get_entryprobability}
\title{Get probabilities to enter the parliament.}
\usage{
get_entryprobability(dirichlet.draws, hurdle = 0.05)
}
\arguments{
\item{dirichlet.draws}{Matrix or data frame containing draws from
the posterior (see \code{\link{draw_from_posterior}}).}

\item{hurdle}{The percentage threshold which has to be reached by a party
to enter the parliament. Any party called "ssw" will be exempt from the hurdle.}
}
\value{
Vector of (named) entry probabilities.
}
\description{
Get probabilities to enter the parliament.
}
\examples{
library(coalitions)
library(dplyr)
# get the latest survey for a sample of German federal election polls
surveys <- get_latest(surveys_sample)
# use 100 simulations for a fast runtime
surveys <- surveys \%>\% mutate(draws = purrr::map(survey, draw_from_posterior, nsim = 100),
                              entryProbs = purrr::map(draws, get_entryprobability))
surveys$entryProbs
}
\seealso{
\code{\link{draw_from_posterior}}
}
\keyword{internal}
