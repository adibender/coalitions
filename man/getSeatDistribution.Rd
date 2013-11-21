\name{getSeatDistribution}
\alias{getSeatDistribution}
\title{Calculate seat distribution from draws from posterior}
\usage{
  getSeatDistribution(dirichlet.draws, survey,
    distrib.fun = sls, sample.size = NULL, ...)
}
\arguments{
  \item{dirichlet.draws}{Matrix containing random draws
  from posterior.}

  \item{survey}{The actual survey results on which
  \code{dirichlet.draws} were based on.}

  \item{distrib.fun}{Function to calculate seat
  distribution. Defaults to \code{\link{sls}}
  (Sainte-Lague/Scheppers).}

  \item{sample.size}{Number of individuals participating in
  the \code{survey}.}

  \item{...}{Further arguments passed to
  \code{distrib.fun}.}
}
\value{
  \code{list} containing seat distributions for each
  simulation in \code{dirichlet.draws}
}
\description{
  Calculate seat distribution from draws from posterior
}
\seealso{
  \code{\link{drawElectionsFromPosterior}},
  \code{\link{sls}}, \code{\link{dHondt}}
}
\keyword{distribution}
\keyword{seat}

