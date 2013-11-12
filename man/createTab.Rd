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
}
\value{
  data.frame containing input and absolute number of votes
  in survey per party
}
\description{
  Creates basic table containing votes (in percent) and
  party names, conducts sanity checks
}
\examples{
test.tab <- createTab( party = c(LETTERS[1:9])
       votes.in.perc = c(40.9, 25.4, 14.3, 4.7, 7.5, 1.9, 1.9, 1.3, 2.1)/100,
       sample.size = 1006)
test.tab
}
\seealso{
  \code{\link{get.props}}
}
\keyword{survey}

