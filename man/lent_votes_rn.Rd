\name{lent_votes_rn}
\alias{lent_votes_rn}
\title{Votes lent from one party to another for strategic reasons but not revealed
in survey.}
\usage{
  lent_votes_rn(rn.mat, lent, from = "CDU/CSU", to = "FDP")
}
\arguments{
  \item{rn.mat}{Matrix containing draws from posterior see
  \code{\link{drawElectionsFromPosterior}}}

  \item{lent}{Percentage of votes lent from one party to
  another}

  \item{from}{Party from which votes are lent}

  \item{to}{Party to which votes are lent}
}
\value{
  \code{rn.mat} adjusted for lent.votes
}
\description{
  Votes lent from one party to another for strategic
  reasons but not revealed in survey.
}
\seealso{
  \code{\link{drawElectionsFromPosterior}}
}

