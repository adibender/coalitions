\name{redistribute}
\alias{redistribute}
\title{Calculate percentage of votes/seats after excluding parties with
\code{votes < hurdle}}
\usage{
  redistribute(survey, hurdle = 0.05)
}
\arguments{
  \item{survey}{table containing votes and votes in percent
  (as created e.g. by \code{\link{as_survey}})}

  \item{hurdle}{the percentage of votes that must be
  reached to get seats in parliament. Defaults to 0.05
  (hurdle for german parliament)}
}
\description{
  Calculate percentage of votes/seats after excluding
  parties with \code{votes < hurdle}
}
\seealso{
  \code{\link{as_survey}}, \code{\link{sls}}
}
\keyword{hurdle}
\keyword{survey,}

