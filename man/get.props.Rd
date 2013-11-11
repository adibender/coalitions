\name{get.props}
\alias{get.props}
\title{Calculate percentage of votes/seats after excluding parties with
\code{votes.in.perc < 0.05}}
\usage{
  get.props(survey, hurdle = 0.05)
}
\arguments{
  \item{survey}{table containing votes and votes.in.perc
  (as created e.g. by \code{\link{createTab}})}

  \item{hurdle}{the percentage of votes that must be
  reached to get seats in parliament. Defaults to 0.05
  (hurdle for german parliament)}
}
\description{
  Calculate percentage of votes/seats after excluding
  parties with \code{votes.in.perc < 0.05}
}
\seealso{
  \code{\link{createTab}}, \code{\link{sls}}
}
\keyword{hurdle}
\keyword{survey,}

