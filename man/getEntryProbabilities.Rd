\name{getEntryProbabilities}
\alias{getEntryProbabilities}
\title{Get probabilities to enter the parliament.}
\usage{
  getEntryProbabilities(dirichlet.draws, hurdle = 0.05)
}
\arguments{
  \item{dirichlet.draws}{Matrix or data frame containing
  draws from the posterior (see
  \code{\link{drawElectionsFromPosterior}}).}

  \item{hurdle}{the percentage of votes that must be
  reached to get seats in parliament. Defaults to 0.05
  (hurdle for german parliament).}

  \item{max.percent.lent,}{from, to see
  \code{\link{lentVotesRn}}.}
}
\value{
  Vector of (named) entry probabilities.
}
\description{
  Get probabilities to enter the parliament.
}
\seealso{
  \code{\link{drawElectionsFromPosterior}},
  \code{\link{lentVotesRn}}
}

