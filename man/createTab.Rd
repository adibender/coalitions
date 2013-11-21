\name{createTab}
\alias{createTab}
\title{Creates basic table containing votes (in percent) and party names, conducts
sanity checks}
\usage{
  createTab(votes.in.perc, sample.size,
    parties = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "AfD", "FW", "Sonstige"),
    epsilon = 1e-05)
}
\arguments{
  \item{votes.in.perc}{votes in percent each party received
  in the survey of interest. Can be set to \code{NA}, if
  parties are specified that are not mentioned in the
  specific survey (otherwise the \code{parties} argument
  has to be modified)}

  \item{sample.size}{number of respondents in survey}

  \item{parties}{vector of same length and in the same
  orderas \code{votes.in.perc}}

  \item{epsilon}{\code{votes.in.perc} should add up to one.
  This parameter controls the maximal numerical divergence
  allowed.}
}
\value{
  data.frame containing input and absolute number of votes
  in survey per party
}
\description{
  Creates basic table containing votes (in percent) and
  party names, conducts sanity checks
}
\seealso{
  \code{\link{get.props}}
}
\keyword{survey}

