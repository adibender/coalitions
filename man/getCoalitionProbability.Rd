\name{getCoalitionProbability}
\alias{getCoalitionProbability}
\title{Calculates coalition probability for one coalition given seat distribution}
\usage{
  getCoalitionProbability(seat.tab, coalition, superior,
    majority = 300)
}
\arguments{
  \item{seat.tab}{\code{data.frame} containing
  seat.distributions from simulations}

  \item{coalition}{the coalition of interest}

  \item{superior}{Superior coalition to \code{coalition}.
  If not \code{NA} majority for coalition is considered
  only if there is no majority for \code{superior}
  coalition.}

  \item{majority}{Number of seats required for majority in
  parliament. Defaults to 300 (required number of seats in
  german parliament).}
}
\value{
  Probability for majority of \code{coalition}.
}
\description{
  Calculates coalition probability for one coalition given
  seat distribution
}
\seealso{
  \code{\link{}}
}

